
# Librerías ----

library(rgdal)        # Versión 1.2-8
library(leaflet)      # Versión 1.1.0 
library(leaflet.extras)
library(htmlwidgets)  # Versión 1.0 
library(tidyverse)    # Versión 1.2.1
library(rjson)        # Versión 0.2.15
library(readxl)       # Versión 1.0.0
library(viridis)      # Versión 0.4.0 
library(extrafont)    # Versión 0.17


# Funciones ----

Salvar <- function(objeto, ruta, nombre){
  saveWidget(objeto,
             file = file.path(str_sub(getwd(), 1, str_length(getwd())-15),
                              ruta,
                              nombre),
             selfcontained = F, libdir = "libraryjs")
  
}

# Saber---- 

# Mt1100 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_Nacional %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# Mt1100 -Map----


# Ubicar el centroide del país

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberpor municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F)) %>% 
  addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE)

municipios2


## Mapa de municipios


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0 , 1 ,  6 ,  11 ,  101 ,  1001 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  35000) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios

### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000))

# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más de 10000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

 for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  Inf ) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 201 ,  501 ,  2001 ,  10001 ,  Inf) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más  de 10000"))%>%
  
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept

# Exportar ----

Salvar(municipios2, "G_Saber/Nal/Matriculados", "Nac_col1.html")
Salvar(municipios, "G_Saber/Nal/Matriculados", "Nac_mun.html")
Salvar(colombia, "G_Saber/Nal/Matriculados", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Nal/Matriculados", "Nac_comb.html")


# Mt1101 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_Bogota %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# Mt1101 -Map----


# Ubicar el centroide del país

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberpor municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


## Mapa de municipios


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0 , 1 ,  6 ,  11 ,  101 ,  1001 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  35000) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios

### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000))

# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más de 10000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  Inf ) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 201 ,  501 ,  2001 ,  10001 ,  Inf) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más  de 10000"))%>%
  
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept

# Exportar ----

Salvar(municipios2, "G_Saber/Bog/Matriculados", "Nac_col1.html")
Salvar(municipios, "G_Saber/Bog/Matriculados", "Nac_mun.html")
Salvar(colombia, "G_Saber/Bog/Matriculados", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Bog/Matriculados", "Nac_comb.html")


# Mt1102 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_Medellin %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# Mt1102 -Map----


# Ubicar el centroide del país

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberpor municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


## Mapa de municipios


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0 , 1 ,  6 ,  11 ,  101 ,  1001 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  35000) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios

### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000))

# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más de 10000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  Inf ) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 201 ,  501 ,  2001 ,  10001 ,  Inf) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más  de 10000"))%>%
  
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept

# Exportar ----

Salvar(municipios2, "G_Saber/Med/Matriculados", "Nac_col1.html")
Salvar(municipios, "G_Saber/Med/Matriculados", "Nac_mun.html")
Salvar(colombia, "G_Saber/Med/Matriculados", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Med/Matriculados", "Nac_comb.html")

# Mt1103 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_Manizales %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# Mt1103 -Map----


# Ubicar el centroide del país

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberpor municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


## Mapa de municipios


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0 , 1 ,  6 ,  11 ,  101 ,  1001 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  35000) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios

### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000))

# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más de 10000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  Inf ) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 201 ,  501 ,  2001 ,  10001 ,  Inf) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más  de 10000"))%>%
  
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept

# Exportar ----

Salvar(municipios2, "G_Saber/Man/Matriculados", "Nac_col1.html")
Salvar(municipios, "G_Saber/Man/Matriculados", "Nac_mun.html")
Salvar(colombia, "G_Saber/Man/Matriculados", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Man/Matriculados", "Nac_comb.html")


# Mt1104 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_Palmira %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# Mt1104 -Map----

# Ubicar el centroide del país

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberpor municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


## Mapa de municipios


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0 , 1 ,  6 ,  11 ,  101 ,  1001 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  35000) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios

### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000))

# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más de 10000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  Inf ) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 201 ,  501 ,  2001 ,  10001 ,  Inf) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más  de 10000"))%>%
  
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept

# Exportar ----

Salvar(municipios2, "G_Saber/Pal/Matriculados", "Nac_col1.html")
Salvar(municipios, "G_Saber/Pal/Matriculados", "Nac_mun.html")
Salvar(colombia, "G_Saber/Pal/Matriculados", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Pal/Matriculados", "Nac_comb.html")


# MATRICULA PREGRADO ---- 

# MtPre1100 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_Pre_Nacional %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPre1100 -Map----


# Ubicar el centroide del país

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberpor municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


## Mapa de municipios


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0 , 1 ,  6 ,  11 ,  101 ,  1001 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  35000) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios

### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000))

# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más de 10000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  Inf ) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 201 ,  501 ,  2001 ,  10001 ,  Inf) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más  de 10000"))%>%
  
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept

# Exportar ----

Salvar(municipios2, "G_Saber/Nal/Pregrado", "Nac_col1.html")
Salvar(municipios, "G_Saber/Nal/Pregrado", "Nac_mun.html")
Salvar(colombia, "G_Saber/Nal/Pregrado", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Nal/Pregrado", "Nac_comb.html")


# MtPre1101 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_Pre_Bogota %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPre1101 -Map----


# Ubicar el centroide del país

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberpor municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


## Mapa de municipios


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0 , 1 ,  6 ,  11 ,  101 ,  1001 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  35000) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios

### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000))

# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más de 10000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  Inf ) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 201 ,  501 ,  2001 ,  10001 ,  Inf) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más  de 10000"))%>%
  
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept

# Exportar ----

Salvar(municipios2, "G_Saber/Bog/Pregrado", "Nac_col1.html")
Salvar(municipios, "G_Saber/Bog/Pregrado", "Nac_mun.html")
Salvar(colombia, "G_Saber/Bog/Pregrado", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Bog/Pregrado", "Nac_comb.html")


# MtPre1102 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_Pre_Medellin %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPre1102 -Map----


# Ubicar el centroide del país

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberpor municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


## Mapa de municipios


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0 , 1 ,  6 ,  11 ,  101 ,  1001 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  35000) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios

### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000))

# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más de 10000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  Inf ) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 201 ,  501 ,  2001 ,  10001 ,  Inf) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más  de 10000"))%>%
  
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept

# Exportar ----

Salvar(municipios2, "G_Saber/Med/Pregrado", "Nac_col1.html")
Salvar(municipios, "G_Saber/Med/Pregrado", "Nac_mun.html")
Salvar(colombia, "G_Saber/Med/Pregrado", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Med/Pregrado", "Nac_comb.html")

# MtPre1103 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_Pre_Manizales %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPre1103 -Map----


# Ubicar el centroide del país

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberpor municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


## Mapa de municipios


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0 , 1 ,  6 ,  11 ,  101 ,  1001 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  35000) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios

### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000))

# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más de 10000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  Inf ) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 201 ,  501 ,  2001 ,  10001 ,  Inf) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más  de 10000"))%>%
  
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept

# Exportar ----

Salvar(municipios2, "G_Saber/Man/Pregrado", "Nac_col1.html")
Salvar(municipios, "G_Saber/Man/Pregrado", "Nac_mun.html")
Salvar(colombia, "G_Saber/Man/Pregrado", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Man/Pregrado", "Nac_comb.html")


# MtPre1104 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_Pre_Palmira %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPre1104 -Map----

# Ubicar el centroide del país

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberpor municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


## Mapa de municipios


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0 , 1 ,  6 ,  11 ,  101 ,  1001 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  35000) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios

### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000))

# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 ,  201 ,  501 ,  2001 ,  10001 ,  35000) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más de 10000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 1 , 6 , 11 , 101 , 1001 ,  Inf ) ,  labels = c("0" ,  "1 - 5" ,  "6 - 10" ,  "11 - 100" ,  "101 - 1000" ,  "Más de 1000") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 201 ,  501 ,  2001 ,  10001 ,  Inf) ,  labels = c("0 - 200 " ,  "201 - 500" ,  "501 - 2000" ,  "2001 - 10000" ,  "Más  de 10000"))%>%
  
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept

# Exportar ----

Salvar(municipios2, "G_Saber/Pal/Pregrado", "Nac_col1.html")
Salvar(municipios, "G_Saber/Pal/Pregrado", "Nac_mun.html")
Salvar(colombia, "G_Saber/Pal/Pregrado", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Pal/Pregrado", "Nac_comb.html")


# MATRICULA POSTGRADO ---- 

# MtPos1100 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_Pos_Nacional %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPos1100 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Nal/Postgrado", "Nac_col1.html")
Salvar(municipios, "G_Saber/Nal/Postgrado", "Nac_mun.html")
Salvar(colombia, "G_Saber/Nal/Postgrado", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Nal/Postgrado", "Nac_comb.html")


# MtPos1101 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_Pos_Bogota %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPos1101 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Bog/Postgrado", "Nac_col1.html")
Salvar(municipios, "G_Saber/Bog/Postgrado", "Nac_mun.html")
Salvar(colombia, "G_Saber/Bog/Postgrado", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Bog/Postgrado", "Nac_comb.html")



# MtPos1102 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_Pos_Medellin %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPos1102 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Med/Postgrado", "Nac_col1.html")
Salvar(municipios, "G_Saber/Med/Postgrado", "Nac_mun.html")
Salvar(colombia, "G_Saber/Med/Postgrado", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Med/Postgrado", "Nac_comb.html")


# MtPos1103 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_Pos_Manizales %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPos1103 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Man/Postgrado", "Nac_col1.html")
Salvar(municipios, "G_Saber/Man/Postgrado", "Nac_mun.html")
Salvar(colombia, "G_Saber/Man/Postgrado", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Man/Postgrado", "Nac_comb.html")


# MtPos1104 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_Pos_Palmira %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPos1104 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Pal/Postgrado", "Nac_col1.html")
Salvar(municipios, "G_Saber/Pal/Postgrado", "Nac_mun.html")
Salvar(colombia, "G_Saber/Pal/Postgrado", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Pal/Postgrado", "Nac_comb.html")


# SaberPVEZ ---- 

# MtPvez1100 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_pvez_Nacional %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPvez1100 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Nal/Mpv", "Nac_col1.html")
Salvar(municipios, "G_Saber/Nal/Mpv", "Nac_mun.html")
Salvar(colombia, "G_Saber/Nal/Mpv", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Nal/Mpv", "Nac_comb.html")


# MtPvez1101 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_pvez_Bogota %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPvez1101 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Bog/Mpv", "Nac_col1.html")
Salvar(municipios, "G_Saber/Bog/Mpv", "Nac_mun.html")
Salvar(colombia, "G_Saber/Bog/Mpv", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Bog/Mpv", "Nac_comb.html")



# MtPvez1102 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_pvez_Medellin %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPvez1102 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Med/Mpv", "Nac_col1.html")
Salvar(municipios, "G_Saber/Med/Mpv", "Nac_mun.html")
Salvar(colombia, "G_Saber/Med/Mpv", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Med/Mpv", "Nac_comb.html")


# MtPvez1103 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_pvez_Manizales %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPvez1103 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Man/Mpv", "Nac_col1.html")
Salvar(municipios, "G_Saber/Man/Mpv", "Nac_mun.html")
Salvar(colombia, "G_Saber/Man/Mpv", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Man/Mpv", "Nac_comb.html")


# MtPvez1104 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_pvez_Palmira %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPvz1104 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Pal/Mpv", "Nac_col1.html")
Salvar(municipios, "G_Saber/Pal/Mpv", "Nac_mun.html")
Salvar(colombia, "G_Saber/Pal/Mpv", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Pal/Mpv", "Nac_comb.html")


# MATRICULA PVEZ PRE ---- 

# MtPvezPre1100 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_pvez_pre_Nacional %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPvezPre1100 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Nal/Mpvpre", "Nac_col1.html")
Salvar(municipios, "G_Saber/Nal/Mpvpre", "Nac_mun.html")
Salvar(colombia, "G_Saber/Nal/Mpvpre", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Nal/Mpvpre", "Nac_comb.html")


# MtPvezPre1101 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_pvez_pre_Bogota %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPvezPre1101 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Bog/Mpvpre", "Nac_col1.html")
Salvar(municipios, "G_Saber/Bog/Mpvpre", "Nac_mun.html")
Salvar(colombia, "G_Saber/Bog/Mpvpre", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Bog/Mpvpre", "Nac_comb.html")



# MtPvezPre1102 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_pvez_pre_Medellin %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPvezPre1102 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Med/Mpvpre", "Nac_col1.html")
Salvar(municipios, "G_Saber/Med/Mpvpre", "Nac_mun.html")
Salvar(colombia, "G_Saber/Med/Mpvpre", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Med/Mpvpre", "Nac_comb.html")


# MtPvezPre1103 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_pvez_pre_Manizales %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPvezPre1103 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Man/Mpvpre", "Nac_col1.html")
Salvar(municipios, "G_Saber/Man/Mpvpre", "Nac_mun.html")
Salvar(colombia, "G_Saber/Man/Mpvpre", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Man/Mpvpre", "Nac_comb.html")


# MtPvezPre1104 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_pvez_pre_Palmira %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPvzPre1104 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Pal/Mpvpre", "Nac_col1.html")
Salvar(municipios, "G_Saber/Pal/Mpvpre", "Nac_mun.html")
Salvar(colombia, "G_Saber/Pal/Mpvpre", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Pal/Mpvpre", "Nac_comb.html")

# MATRICULA PVEZ POS ---- 

# MtPvezPos1100 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_pvez_pos_Nacional %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPvezPos1100 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Nal/Mpvpos", "Nac_col1.html")
Salvar(municipios, "G_Saber/Nal/Mpvpos", "Nac_mun.html")
Salvar(colombia, "G_Saber/Nal/Mpvpos", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Nal/Mpvpos", "Nac_comb.html")


# MtPvezPos1101 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_pvez_pos_Bogota %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPvezPos1101 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Bog/Mpvpos", "Nac_col1.html")
Salvar(municipios, "G_Saber/Bog/Mpvpos", "Nac_mun.html")
Salvar(colombia, "G_Saber/Bog/Mpvpos", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Bog/Mpvpos", "Nac_comb.html")



# MtPvezPos1102 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_pvez_pos_Medellin %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPvezPos1102 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Med/Mpvpos", "Nac_col1.html")
Salvar(municipios, "G_Saber/Med/Mpvpos", "Nac_mun.html")
Salvar(colombia, "G_Saber/Med/Mpvpos", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Med/Mpvpos", "Nac_comb.html")


# MtPvezPos1103 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_pvez_pos_Manizales %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPvezPos1103 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Man/Mpvpos", "Nac_col1.html")
Salvar(municipios, "G_Saber/Man/Mpvpos", "Nac_mun.html")
Salvar(colombia, "G_Saber/Man/Mpvpos", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Man/Mpvpos", "Nac_comb.html")


# MtPvezPos1104 -Imp----

# Importar Divipola

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Población ----

Matricula <- Mat_pvez_pos_Palmira %>% filter(YEAR == ano, SEMESTRE == semestre)

# Inicio preparación datos mapas

Saber<- Matricula %>% select(depart_asp=DEP_NAC,
                                     codept_asp=COD_DEP_NAC,
                                     ciudad_asp=CIU_NAC,
                                     codecity_asp=COD_CIU_NAC,
                                     long_asp=LON_CIU_NAC,
                                     lat_asp=LAT_CIU_NAC) %>%
  filter(!is.na(depart_asp))


# Total de Saberpor departamento de nacimiento 

CANT_MAT <- Saber%>% group_by(codept_asp) %>% summarise(Total=n())


# Total de Saberpor municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- Saber%>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))


# Extraer lista de códigos de los municipios - 1122 - municipios


# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de Saberpor municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_MAT <- codigos[ ,2]


#Lectura de JSON de Colombia por departamentos


# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de Saberpor departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in CANT_MAT$codept_asp){
  codigos2[codigos2[,1] == i, 2] = CANT_MAT$Total[CANT_MAT$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de Saberpor depto de nacimiento

colombia.R@data$CANT_MAT <- codigos2[,2]


# Componente final de mapas



# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoquía", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# MtPvzPos1104 -Map----

centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")

# Paleta para mapa de Saberen postgrado por municipio (0 y 1)

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)

for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_MAT ,  bins = c(0 , 1 ,  34750) , 
            title = paste0("Matriculados", periodo_actual_titulo), labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

municipios2


### Mapa de municipios 


# Paleta para mapa de aspirantes por municipio de nacimiento

pal_col <- colorBin(palette = "YlGn" , bins = c(0, 1, 2, 4, 11, 101, Inf))

# Etiquetas para el mapa interactivo

labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

municipios <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  municipios <- municipios %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios <- municipios %>%
  
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>% 
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2)%>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = capitales$latitud ,  lng = capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))

municipios


##### Mapa departamentos 

# Paleta para mapa de aspirantes por departamento de nacimiento

pal_col2 <- colorBin(palette = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') , bins = c(0 ,  21 ,  51 ,  201 ,  1001 ,  Inf))


# Etiquetas para el mapa interactivo

labels <- sprintf(
  "<strong>%s</strong><br/>%g matriculados" , 
  colombia.R@data$NOMBRE_DPT ,  colombia.R@data$CANT_MAT
) %>% lapply(htmltools::HTML)

# Inicio mapa

colombia  <- leaflet(data = colombia.R)

for (k in c(1 , 2 , 3)) {
  colombia <- colombia %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
} 

colombia <- colombia %>% addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                                          options = layersControlOptions(collapsed = FALSE)) %>%
  
  setView(lng = centroidecol$lon ,  lat = centroidecol$lat ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,  labelOptions = labelOptions(style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste0("Matriculados", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más de 1000"))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =   ~paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8px") )%>%
  addLabelOnlyMarkers(lat = centro_dept$lat ,  lng = centro_dept$lon , label =   ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))

colombia


#### Mapa combinado departamentos-municipios 

# Inicio mapa

mundept <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 7)

for (k in c(1 , 2 , 3)) {
  mundept <- mundept %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

mundept <- mundept %>% 
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c("Mostrar <br> Departamentos" ,   "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE ,  autoZindex =  T)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_col(CANT_MAT) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.7 , 
                bringToFront = F) ,  group =  "Municipios") %>% 
  addPolygons(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  fillOpacity = 1 ,  color = '#005a32' ,  weight = 2 , 
              fillColor = ~pal_col2(CANT_MAT) , 
              label = labels ,   labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                 
                fillOpacity = 0.05 , 
                bringToFront = F) ,  group = "Mostrar <br> Departamentos" ,  options = easyButton(
                  icon = "glyphicon-screenshot" ,  title = "Retornar" , 
                  onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")) )%>%
  showGroup("Municipios")%>%
  hideGroup("Mostrar <br> Departamentos")%>%
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = '#005a32' ,  weight = 2 ,  group = "Municipios")%>%
  
  addLegend("bottomright" ,  colors = c('#ffffcc' , '#d9f0a3' , '#addd8e' , '#78c679' , '#31a354' , '#006837') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "municipio", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0, 1, 2, 4, 11, 101, Inf) ,  labels = c("0" ,  "1" ,  "2 - 3" ,  "4 - 10" ,  "11 - 100" ,  "Más de 100") ,  layerId = )%>%
  addLegend("bottomright" ,  colors = c('#d0d1e6' , '#a6bddb' , '#74a9cf' , '#2b8cbe' , '#045a8d') ,  values = ~CANT_MAT ,  
            title = paste("Saberpor", "<br>", "departamento", "<br>", periodo_actual_titulo), 
            opacity = 1 ,  bins = c(0 , 21 , 51 , 201 , 1001 , Inf) ,  labels = c("0 - 20 " ,  "21 - 50" ,  "51 - 200" ,  "201 - 1000" ,  "Más  de 1000"))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px" ,  clickable = TRUE) ,  options = markerOptions(riseOnHover =  TRUE ,  clickable = TRUE) )%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Mostrar <br> Departamentos")%>%
  addCircleMarkers(radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color = '#d95f02' , fill =  T ,  fillColor = "orangelight"  , lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  group = "Municipios")%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$latitud ,  lng = sedes$longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addLabelOnlyMarkers(lat = ~capitales$latitud ,  lng = ~capitales$longitud ,  label =  paste0(sapply(tolower(capitales$municipios) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  textOnly = T , textsize = "8.5px") ,  options = markerOptions(riseOnHover =  TRUE) ,  group = "Departamentos")

mundept


# Exportar ----

Salvar(municipios2, "G_Saber/Pal/Mpvpos", "Nac_col1.html")
Salvar(municipios, "G_Saber/Pal/Mpvpos", "Nac_mun.html")
Salvar(colombia, "G_Saber/Pal/Mpvpos", "Nac_dpto.html")
Salvar(mundept, "G_Saber/Pal/Mpvpos", "Nac_comb.html")


