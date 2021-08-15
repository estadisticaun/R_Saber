
# INICIO FASE DE VISUALIZACIÓN

# Librerías requeridas ----

library(tidyverse)   # version 1.2.1
library(readxl)      # version 1.0.0
library(DT)          # version 0.4
library(highcharter) # version 0.5.0.9999
library(treemap) # version 2.4-2


# Importar - scrips ----

# source("R/Importar Matriculados.R", encoding = 'UTF-8')

source("R/Funciones.R", encoding = 'UTF-8')


# Funciones ----

# Exportar archivos HTML


Salvar <- function(objeto, ruta, nombre){
  saveWidget(objeto,
             file = file.path(str_sub(getwd(), 1, str_length(getwd())-15),
                              ruta,
                              nombre),
             selfcontained = F, libdir = "libraryjs")
  
      }


# MATRICULADOS ---- 

# Mt1100 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "Mt1100") %>% select(-(Nivel))


# Evolución histórica  ---


col <-   c("#0071bc") # Azul vivo, Total

EVOLUCION <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados", eje = "Número de estudiantes (k: miles)");EVOLUCION


# Modalidad de formación  ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado

TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes matriculados por modalidad de formación', mensaje = "Número de estudiantes matriculados por modalidad de formación", titulo = "Matriculados por modalidad de formación");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por modalidad de formación", eje = "Número de estudiantes  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col,  titulo = "Distribución de matriculados por modalidad de formación", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL




# Nivel de formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#fbb03b", # amarillo, Especialidades médicas
            "#29abe2", # azul claro, Especialización
            "#c1272d",  # rojo, Maestría
            "#8cc63f",  # verde, Pregrado
            "#93278f") # morado, Tecnología

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados por nivel de formación', mensaje = "Número de estudiantes matriculados por nivel de formación", titulo = "Matriculados por nivel de formación");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por nivel de formación", eje = "Número de estudiantes  (k: miles)");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por nivel de formación", eje = "Número de estudiantes (k: miles)"); NIVEL_ACTUAL


# Sedes ---

col <-   c( "#29abe2", # azul claro, Amazonía
            "#8cc63f", # verde, Bogotá
            "#c1272d", # rojo, Caribe 
            "#9e9ac8",  # Morado claro, De la Paz
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellín
            "#fbb03b", # amarillo, Orinoquía 
            "#93278f", # Morado, Palmira
            "#6d6666"  # gris, Tumaco 
) 

SEDE_NOMBRE_MAT_TABLA <- tabla(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", variable = 'Total estudiantes matriculados por sedes', mensaje = "Total de estudiantes matriculados por sedes", titulo = "Sede estudiantes matriculados");SEDE_NOMBRE_MAT_TABLA
SEDE_NOMBRE_MAT_SERIE <- series(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", colores = col, titulo = "Evolución del número de estudiantes matriculados por sede", eje = "Número de estudiantes (k: miles)");SEDE_NOMBRE_MAT_SERIE
SEDE_NOMBRE_MAT_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por sede", eje = "Número de estudiantes (k: miles)"); SEDE_NOMBRE_MAT_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados según nacionalidad', mensaje = "Número de estudiantes matriculados por nacionalidad", titulo = "Matriculados según nacionalidad");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados según nacionalidad", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados según la nacionalidad", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---


col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por sexo', mensaje = "Número de estudiantes por sexo", titulo = "Estudiantes por sexo");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por sexo", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado,  variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por sexo", etiqueta = "Número de estudiantes", ano = ano,  periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Matriculados prim. vez ---


col <-   c( "#0071bc", # azul vivo, 
            "#8cc63f") # verde, 


MAT_PVEZ_TABLA <- tabla(datos = Consolidado, categoria = "MAT_PVEZ", variable = 'Total estudiantes matriculados por primera vez', mensaje = "Número de estudiantes matriculados por primera vez", titulo = "Estudiantes matriculados por primera vez");MAT_PVEZ_TABLA
MAT_PVEZ_SERIE <- series(datos = Consolidado, categoria = "MAT_PVEZ", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez", eje = "Número de estudiantes (k: miles)");MAT_PVEZ_SERIE
MAT_PVEZ_ACTUAL <- torta(datos = Consolidado, variable = "MAT_PVEZ", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MAT_PVEZ_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales   


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por áreas del conocimiento SNIES', mensaje = "Total de estudiantes matriculados por áreas del conocimiento SNIES", titulo = "Sede estudiantes matriculados por áreas del conocimiento SNIES");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por áreas del conocimiento SNIES", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por áreas del conocimiento SNIES", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION, "G_Matriculados/Nal/Matriculados", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Matriculados/Nal/Matriculados", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Matriculados/Nal/Matriculados", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Matriculados/Nal/Matriculados", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Nal/Matriculados", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Nal/Matriculados", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Nal/Matriculados", "A_nivel.html")
Salvar(SEDE_NOMBRE_MAT_TABLA, "G_Matriculados/Nal/Matriculados", "T_sede.html")
Salvar(SEDE_NOMBRE_MAT_SERIE, "G_Matriculados/Nal/Matriculados", "S_sede.html")
Salvar(SEDE_NOMBRE_MAT_ACTUAL, "G_Matriculados/Nal/Matriculados", "A_sede.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Nal/Matriculados", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Nal/Matriculados", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Nal/Matriculados", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Nal/Matriculados", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Nal/Matriculados", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Nal/Matriculados", "A_sexo.html")
Salvar(MAT_PVEZ_TABLA, "G_Matriculados/Nal/Matriculados", "T_mpv.html")
Salvar(MAT_PVEZ_SERIE, "G_Matriculados/Nal/Matriculados", "S_mpv.html")
Salvar(MAT_PVEZ_ACTUAL, "G_Matriculados/Nal/Matriculados", "A_mpv.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Nal/Matriculados", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Nal/Matriculados", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Nal/Matriculados", "A_snies.html")


# Mt1101 ---- 

# Base de datos agregada - Sede Bogotá

Consolidado <- ConsolidadoG %>% filter(Nivel == "Mt1101") %>% select(-(Nivel))


# Evolución histórica  ---


col <-   c("#8cc63f") # Verde, Total sede Bogotá

EVOLUCION <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados - sede Bogotá", eje = "Número de estudiantes (k: miles)");EVOLUCION


# Modalidad de formación  ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado

TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes matriculados por modalidad de formación - sede Bogotá', mensaje = "Número de estudiantes matriculados por modalidad de formación - sede Bogotá", titulo = "Matriculados por modalidad de formación - sede Bogotá");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por modalidad de formación - sede Bogotá", eje = "Número de estudiantes  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col,  titulo = "Distribución de matriculados por modalidad de formación - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#fbb03b", # amarillo, Especialidades médicas
            "#29abe2", # azul claro, Especialización
            "#c1272d",  # rojo, Maestría
            "#8cc63f",  # verde, Pregrado
            "#93278f") # morado, Tecnología

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados por nivel de formación - sede Bogotá', mensaje = "Número de estudiantes matriculados por nivel de formación - sede Bogotá", titulo = "Matriculados por nivel de formación - sede Bogotá");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por nivel de formación - sede Bogotá", eje = "Número de estudiantes  (k: miles)");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por nivel de formación - sede Bogotá", eje = "Número de estudiantes (k: miles)"); NIVEL_ACTUAL


# Facultad ---

col <-   c( "#9e9ac8",  # Morado claro, Enfermería
            "#0071bc", # azul vivo, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias económicas
            "#f15a24", # naranja, Ciencias humanas
            "#fbb03b", # amarillo, Derecho, ciencias políticas y sociales
            "#93278f", # Morado, Artes 
            "#8cc63f", # verde, Ingeniería
            "#bdbdbd",  # Gris claro, Medicina 
            "#c1272d", # rojo, Medicina veterianaria y zootecnia
            "#99d8c9"  # Agua Marina, Odontología 
            
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados por facultad - sede Bogotá', mensaje = "Total de estudiantes matriculados por facultad - sede Bogotá", titulo = "Facultad estudiantes matriculados - sede Bogotá");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por facultad - sede Bogotá", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por facultad - sede Bogotá", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados según nacionalidad - sede Bogotá', mensaje = "Número de estudiantes matriculados por nacionalidad - sede Bogotá", titulo = "Matriculados según nacionalidad - sede Bogotá");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados según nacionalidad - sede Bogotá", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados según la nacionalidad - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---


col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por sexo - sede Bogotá', mensaje = "Número de estudiantes por sexo - sede Bogotá", titulo = "Estudiantes por sexo - sede Bogotá");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por sexo - sede Bogotá", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado,  variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por sexo - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano,  periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Matriculados prim. vez ---


col <-   c( "#0071bc", # azul vivo, 
            "#8cc63f") # verde, 


MAT_PVEZ_TABLA <- tabla(datos = Consolidado, categoria = "MAT_PVEZ", variable = 'Total estudiantes matriculados por primera vez - sede Bogotá', mensaje = "Número de estudiantes matriculados por primera vez - sede Bogotá", titulo = "Estudiantes matriculados por primera vez - sede Bogotá");MAT_PVEZ_TABLA
MAT_PVEZ_SERIE <- series(datos = Consolidado, categoria = "MAT_PVEZ", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez - sede Bogotá", eje = "Número de estudiantes (k: miles)");MAT_PVEZ_SERIE
MAT_PVEZ_ACTUAL <- torta(datos = Consolidado, variable = "MAT_PVEZ", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MAT_PVEZ_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales   


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por áreas del conocimiento SNIES - sede Bogotá', mensaje = "Total de estudiantes matriculados por áreas del conocimiento SNIES - sede Bogotá", titulo = "Sede estudiantes matriculados por áreas del conocimiento SNIES - sede Bogotá");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado %>% filter(is.na(Clase)==FALSE), categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----

Salvar(EVOLUCION, "G_Matriculados/Bog/Matriculados", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Matriculados/Bog/Matriculados", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Matriculados/Bog/Matriculados", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Matriculados/Bog/Matriculados", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Bog/Matriculados", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Bog/Matriculados", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Bog/Matriculados", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Bog/Matriculados", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Bog/Matriculados", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Bog/Matriculados", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Bog/Matriculados", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Bog/Matriculados", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Bog/Matriculados", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Bog/Matriculados", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Bog/Matriculados", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Bog/Matriculados", "A_sexo.html")
Salvar(MAT_PVEZ_TABLA, "G_Matriculados/Bog/Matriculados", "T_mpv.html")
Salvar(MAT_PVEZ_SERIE, "G_Matriculados/Bog/Matriculados", "S_mpv.html")
Salvar(MAT_PVEZ_ACTUAL, "G_Matriculados/Bog/Matriculados", "A_mpv.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Bog/Matriculados", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Bog/Matriculados", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Bog/Matriculados", "A_snies.html")


# Mt1102 ---- 

# Base de datos agregada - Sede Medellín

Consolidado <- ConsolidadoG %>% filter(Nivel == "Mt1102") %>% select(-(Nivel))


# Evolución histórica  ---


col <-   c("#f15a24") # Naranja, Total sede Medellín

EVOLUCION <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados - sede Medellín", eje = "Número de estudiantes (k: miles)");EVOLUCION


# Modalidad de formación  ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado

TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes matriculados por modalidad de formación - sede Medellín', mensaje = "Número de estudiantes matriculados por modalidad de formación - sede Medellín", titulo = "Matriculados por modalidad de formación - sede Medellín");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por modalidad de formación - sede Medellín", eje = "Número de estudiantes  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col,  titulo = "Distribución de matriculados por modalidad de formación - sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d", # rojo, Maestría
            "#8cc63f",  # verde, Pregrado
            "#93278f", # morado, Tecnología
            "#fbb03b")  # amarillo, Especialidades médicas
             

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados por nivel de formación - sede Medellín', mensaje = "Número de estudiantes matriculados por nivel de formación - sede Medellín", titulo = "Matriculados por nivel de formación - sede Medellín");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por nivel de formación - sede Medellín", eje = "Número de estudiantes  (k: miles)");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por nivel de formación - sede Medellín", eje = "Número de estudiantes (k: miles)"); NIVEL_ACTUAL


# Facultad ---

col <-   c( "#9e9ac8",  # Morado claro, Arquitectura
            "#f15a24", # naranja, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias humanas y económicas 
            "#8cc63f" # verde, Minas
            )

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados por facultad - sede Medellín', mensaje = "Total de estudiantes matriculados por facultad - sede Medellín", titulo = "Facultad estudiantes matriculados - sede Medellín");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por facultad - sede Medellín", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por facultad - sede Medellín", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados según nacionalidad - sede Medellín', mensaje = "Número de estudiantes matriculados por nacionalidad - sede Medellín", titulo = "Matriculados según nacionalidad - sede Medellín");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados según nacionalidad - sede Medellín", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados según la nacionalidad - sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---


col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por sexo - sede Medellín', mensaje = "Número de estudiantes por sexo - sede Medellín", titulo = "Estudiantes por sexo - sede Medellín");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por sexo - sede Medellín", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado,  variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por sexo - sede Medellín", etiqueta = "Número de estudiantes", ano = ano,  periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Matriculados prim. vez ---


col <-   c( "#0071bc", # azul vivo, 
            "#8cc63f") # verde, 


MAT_PVEZ_TABLA <- tabla(datos = Consolidado, categoria = "MAT_PVEZ", variable = 'Total estudiantes matriculados por primera vez - sede Medellín', mensaje = "Número de estudiantes matriculados por primera vez - sede Medellín", titulo = "Estudiantes matriculados por primera vez - sede Medellín");MAT_PVEZ_TABLA
MAT_PVEZ_SERIE <- series(datos = Consolidado, categoria = "MAT_PVEZ", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez - sede Medellín", eje = "Número de estudiantes (k: miles)");MAT_PVEZ_SERIE
MAT_PVEZ_ACTUAL <- torta(datos = Consolidado, variable = "MAT_PVEZ", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez - sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MAT_PVEZ_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales   


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por áreas del conocimiento SNIES - sede Medellín', mensaje = "Total de estudiantes matriculados por áreas del conocimiento SNIES - sede Medellín", titulo = "Sede estudiantes matriculados por áreas del conocimiento SNIES - sede Medellín");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por áreas del conocimiento SNIES - sede Medellín", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado %>% filter(is.na(Clase)==FALSE), categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por áreas del conocimiento SNIES - sede Medellín", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----

Salvar(EVOLUCION, "G_Matriculados/Med/Matriculados", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Matriculados/Med/Matriculados", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Matriculados/Med/Matriculados", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Matriculados/Med/Matriculados", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Med/Matriculados", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Med/Matriculados", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Med/Matriculados", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Med/Matriculados", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Med/Matriculados", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Med/Matriculados", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Med/Matriculados", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Med/Matriculados", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Med/Matriculados", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Med/Matriculados", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Med/Matriculados", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Med/Matriculados", "A_sexo.html")
Salvar(MAT_PVEZ_TABLA, "G_Matriculados/Med/Matriculados", "T_mpv.html")
Salvar(MAT_PVEZ_SERIE, "G_Matriculados/Med/Matriculados", "S_mpv.html")
Salvar(MAT_PVEZ_ACTUAL, "G_Matriculados/Med/Matriculados", "A_mpv.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Med/Matriculados", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Med/Matriculados", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Med/Matriculados", "A_snies.html")


# Mt1103 ---- 

# Base de datos agregada - Sede Manizales

Consolidado <- ConsolidadoG %>% filter(Nivel == "Mt1103") %>% select(-(Nivel))


# Evolución histórica  ---


col <-   c("#0071bc") # Azul vivo, Total sede Manizales

EVOLUCION <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados - sede Manizales", eje = "Número de estudiantes (k: miles)");EVOLUCION


# Modalidad de formación  ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado

TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes matriculados por modalidad de formación - sede Manizales', mensaje = "Número de estudiantes matriculados por modalidad de formación - sede Manizales", titulo = "Matriculados por modalidad de formación - sede Manizales");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por modalidad de formación - sede Manizales", eje = "Número de estudiantes  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col,  titulo = "Distribución de matriculados por modalidad de formación - sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d",  # rojo, Maestría
            "#8cc63f",  # verde, Pregrado
            "#fbb03b", # amarillo, Especialidades médicas
            "#93278f") # morado, Tecnología

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados por nivel de formación - sede Manizales', mensaje = "Número de estudiantes matriculados por nivel de formación - sede Manizales", titulo = "Matriculados por nivel de formación - sede Manizales");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por nivel de formación - sede Manizales", eje = "Número de estudiantes  (k: miles)");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por nivel de formación - sede Manizales", eje = "Número de estudiantes (k: miles)"); NIVEL_ACTUAL


# Facultad ---

col <-   c( "#f15a24", # naranja, Administración
            "#0071bc", # azul vivo, Ciencias exactas y naturales
            "#8cc63f" # verde, Ingeniaría y arquitectura
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados por facultad - sede Manizales', mensaje = "Total de estudiantes matriculados por facultad - sede Manizales", titulo = "Facultad estudiantes matriculados - sede Manizales");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por facultad - sede Manizales", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por facultad - sede Manizales", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados según nacionalidad - sede Manizales', mensaje = "Número de estudiantes matriculados por nacionalidad - sede Manizales", titulo = "Matriculados según nacionalidad - sede Manizales");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados según nacionalidad - sede Manizales", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados según la nacionalidad - sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---


col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por sexo - sede Manizales', mensaje = "Número de estudiantes por sexo - sede Manizales", titulo = "Estudiantes por sexo - sede Manizales");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por sexo - sede Manizales", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado,  variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por sexo - sede Manizales", etiqueta = "Número de estudiantes", ano = ano,  periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Matriculados prim. vez ---


col <-   c( "#0071bc", # azul vivo, 
            "#8cc63f") # verde, 


MAT_PVEZ_TABLA <- tabla(datos = Consolidado, categoria = "MAT_PVEZ", variable = 'Total estudiantes matriculados por primera vez - sede Manizales', mensaje = "Número de estudiantes matriculados por primera vez - sede Manizales", titulo = "Estudiantes matriculados por primera vez - sede Manizales");MAT_PVEZ_TABLA
MAT_PVEZ_SERIE <- series(datos = Consolidado, categoria = "MAT_PVEZ", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez - sede Manizales", eje = "Número de estudiantes (k: miles)");MAT_PVEZ_SERIE
MAT_PVEZ_ACTUAL <- torta(datos = Consolidado, variable = "MAT_PVEZ", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez - sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MAT_PVEZ_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales   


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por áreas del conocimiento SNIES - sede Manizales', mensaje = "Total de estudiantes matriculados por áreas del conocimiento SNIES - sede Manizales", titulo = "Sede estudiantes matriculados por áreas del conocimiento SNIES - sede Manizales");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por áreas del conocimiento SNIES - sede Manizales", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado %>% filter(is.na(Clase)==FALSE), categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por áreas del conocimiento SNIES - sede Manizales", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION, "G_Matriculados/Man/Matriculados", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Matriculados/Man/Matriculados", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Matriculados/Man/Matriculados", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Matriculados/Man/Matriculados", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Man/Matriculados", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Man/Matriculados", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Man/Matriculados", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Man/Matriculados", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Man/Matriculados", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Man/Matriculados", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Man/Matriculados", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Man/Matriculados", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Man/Matriculados", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Man/Matriculados", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Man/Matriculados", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Man/Matriculados", "A_sexo.html")
Salvar(MAT_PVEZ_TABLA, "G_Matriculados/Man/Matriculados", "T_mpv.html")
Salvar(MAT_PVEZ_SERIE, "G_Matriculados/Man/Matriculados", "S_mpv.html")
Salvar(MAT_PVEZ_ACTUAL, "G_Matriculados/Man/Matriculados", "A_mpv.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Man/Matriculados", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Man/Matriculados", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Man/Matriculados", "A_snies.html")


# Mt1104 ----

# Base de datos agregada - Sede Palmira

Consolidado <- ConsolidadoG %>% filter(Nivel == "Mt1104") %>% select(-(Nivel))


# Evolución histórica  ---


col <-   c("#93278f") # Morado, Total sede Palmira

EVOLUCION <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados - sede Palmira", eje = "Número de estudiantes (k: miles)");EVOLUCION


# Modalidad de formación  ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado

TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes matriculados por modalidad de formación - sede Palmira', mensaje = "Número de estudiantes matriculados por modalidad de formación - sede Palmira", titulo = "Matriculados por modalidad de formación - sede Palmira");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por modalidad de formación - sede Palmira", eje = "Número de estudiantes  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col,  titulo = "Distribución de matriculados por modalidad de formación - sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d",  # rojo, Maestría
            "#8cc63f",  # verde, Pregrado
            "#fbb03b", # amarillo, Especialidades médicas
            "#93278f") # morado, Tecnología

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados por nivel de formación - sede Palmira', mensaje = "Número de estudiantes matriculados por nivel de formación - sede Palmira", titulo = "Matriculados por nivel de formación - sede Palmira");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por nivel de formación - sede Palmira", eje = "Número de estudiantes  (k: miles)");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por nivel de formación - sede Palmira", eje = "Número de estudiantes (k: miles)"); NIVEL_ACTUAL


# Facultad ---

col <-   c( "#0071bc", # azul vivo, Ciencias agropecuarias 
            "#8cc63f" # verde, Ingeniería y Administración
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados por facultad - sede Palmira', mensaje = "Total de estudiantes matriculados por facultad - sede Palmira", titulo = "Facultad estudiantes matriculados - sede Palmira");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por facultad - sede Palmira", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por facultad - sede Palmira", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados según nacionalidad - sede Palmira', mensaje = "Número de estudiantes matriculados por nacionalidad - sede Palmira", titulo = "Matriculados según nacionalidad - sede Palmira");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados según nacionalidad - sede Palmira", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados según la nacionalidad - sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---


col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por sexo - sede Palmira', mensaje = "Número de estudiantes por sexo - sede Palmira", titulo = "Estudiantes por sexo - sede Palmira");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por sexo - sede Palmira", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado,  variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por sexo - sede Palmira", etiqueta = "Número de estudiantes", ano = ano,  periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Matriculados prim. vez ---


col <-   c( "#0071bc", # azul vivo, 
            "#8cc63f") # verde, 


MAT_PVEZ_TABLA <- tabla(datos = Consolidado, categoria = "MAT_PVEZ", variable = 'Total estudiantes matriculados por primera vez - sede Palmira', mensaje = "Número de estudiantes matriculados por primera vez - sede Palmira", titulo = "Estudiantes matriculados por primera vez - sede Palmira");MAT_PVEZ_TABLA
MAT_PVEZ_SERIE <- series(datos = Consolidado, categoria = "MAT_PVEZ", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez - sede Palmira", eje = "Número de estudiantes (k: miles)");MAT_PVEZ_SERIE
MAT_PVEZ_ACTUAL <- torta(datos = Consolidado, variable = "MAT_PVEZ", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez - sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MAT_PVEZ_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales   


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por áreas del conocimiento SNIES - sede Palmira', mensaje = "Total de estudiantes matriculados por áreas del conocimiento SNIES - sede Palmira", titulo = "Sede estudiantes matriculados por áreas del conocimiento SNIES - sede Palmira");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por áreas del conocimiento SNIES - sede Palmira", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado %>% filter(is.na(Clase)==FALSE), categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por áreas del conocimiento SNIES - sede Palmira", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION, "G_Matriculados/Pal/Matriculados", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Matriculados/Pal/Matriculados", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Matriculados/Pal/Matriculados", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Matriculados/Pal/Matriculados", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Pal/Matriculados", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Pal/Matriculados", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Pal/Matriculados", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Pal/Matriculados", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Pal/Matriculados", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Pal/Matriculados", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Pal/Matriculados", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Pal/Matriculados", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Pal/Matriculados", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Pal/Matriculados", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Pal/Matriculados", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Pal/Matriculados", "A_sexo.html")
Salvar(MAT_PVEZ_TABLA, "G_Matriculados/Pal/Matriculados", "T_mpv.html")
Salvar(MAT_PVEZ_SERIE, "G_Matriculados/Pal/Matriculados", "S_mpv.html")
Salvar(MAT_PVEZ_ACTUAL, "G_Matriculados/Pal/Matriculados", "A_mpv.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Pal/Matriculados", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Pal/Matriculados", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Pal/Matriculados", "A_snies.html")




# MATRICULA PREGRADO ---- 


# Mt_Pre1100 ----


# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPre1100") %>% select(-(Nivel))


# Evolución histórica---


col <-   c("#8cc63f") # verde, Total

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados en pregrado ", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE



# Sedes  ---


col <-   c( "#29abe2", # azul claro, Amazonía
            "#8cc63f", # verde, Bogotá
            "#c1272d", # rojo, Caribe 
            "#9e9ac8",  # Morado claro, De la Paz
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellín
            "#fbb03b", # amarillo, Orinoquía 
            "#93278f", # Morado, Palmira
            "#6d6666"  # gris, Tumaco 
) 


SEDE_NOMBRE_MAT_TABLA <- tabla(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", variable = 'Total estudiantes en pregrado por sede de matrícula', mensaje = "Total de estudiantes en pregrado por sede de matrícula", titulo = "Sede estudiantes matriculados pregrado");SEDE_NOMBRE_MAT_TABLA
SEDE_NOMBRE_MAT_SERIE <- series(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", colores = col, titulo = "Evolución del número de estudiantes en pregrado por sede de matrícula", eje = "Número de estudiantes (k: miles)");SEDE_NOMBRE_MAT_SERIE
SEDE_NOMBRE_MAT_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT",colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes en pregrado por sede de matrícula", eje = "Número de estudiantes"); SEDE_NOMBRE_MAT_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información

NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados en pregrado según nacionalidad', mensaje = "Número de estudiantes matriculados en pregrado por nacionalidad", titulo = "Matriculados según nacionalidad");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado según nacionalidad", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados en pregrado según la nacionalidad", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL

# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados en pregrado por sexo', mensaje = "Número de estudiantes de pregrado por sexo", titulo = "Estudiantes de pregrado por sexo");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por sexo", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado por sexo", etiqueta = "Número de estudiantes",ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados en pregrado por grupos de edad', mensaje = "Número de matriculados en pregrado por grupos de edad", titulo = "Matriculados en pregrado por grupos de edad");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por grupos de edad", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por grupos de edad", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL


# Estrato ---

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)


ESTRATO_TABLA <- tabla(datos = Consolidado, categoria = "ESTRATO", variable = 'Total de estudiantes matriculados en pregrado por estrato socioeconómico', mensaje = "Número de matriculados en pregrado por estrato", titulo = "Matriculados en pregrado por estrato");ESTRATO_TABLA
ESTRATO_SERIE <- series(datos = Consolidado, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por estrato socioeconómico", eje = "Número de estudiantes (k: miles)");ESTRATO_SERIE
ESTRATO_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por estrato", eje = "Número de estudiantes (k: miles)"); ESTRATO_ACTUAL


# Colegio ---

col <-   c( "#8cc63f", # verde, Oficial
            "#0071bc", # azul vivo, Otros 
            "#f15a24", # naranja, Privado 
            "#6d6666" # gris, Sin información
)


################ 1. Tabla

TIPO_COL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_COL", variable = 'Total estudiantes matriculados en pregrado según la naturaleza del colegio', mensaje = "Número de matriculados en pregrado según naturaleza del colegio", titulo = "Matriculados en pregrado según naturaleza del colegio");TIPO_COL_TABLA
TIPO_COL_SERIE <- series(datos = Consolidado, categoria = "TIPO_COL", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por naturaleza del colegio", eje = "Número de estudiantes (k: miles)");TIPO_COL_SERIE
TIPO_COL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_COL", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado según naturaleza del colegio", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_COL_ACTUAL


# PBM ---

col <-   c( "#8cc63f", # verde, 11 o menos
            "#6d6666", # gris, 12 a 17
            "#f15a24", # naranja, 18 a 50 
            "#0071bc", # azul vivo, 51 a 100
            "#93278f" # Morado, Sin información
)


PBM_TABLA <- tabla(datos = Consolidado, categoria = "PBM", variable = 'Total estudiantes matriculados en pregrado según Puntaje Básico Matrícula (PBM)', mensaje = "Número de matriculados en pregrado según Puntaje Básico Matrícula (PBM)", titulo = "Matriculados en pregrado según Puntaje Básico Matrícula (PBM)");PBM_TABLA
PBM_SERIE <- series(datos = Consolidado, categoria = "PBM", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por grupos en Puntaje Básico Matrícula (PBM)", eje = "Número de estudiantes (k: miles)");PBM_SERIE
PBM_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "PBM", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por grupos de Puntaje Básico Matrícula (PBM)", eje = "Número de estudiantes (k: miles)"); PBM_ACTUAL


# Matriculados primera vez ---


col <-   c( "#0071bc", # azul vivo, 
            "#8cc63f") # verde, 

MAT_PVEZ_TABLA <- tabla(datos = Consolidado, categoria = "MAT_PVEZ", variable = 'Total estudiantes de pregrado matriculados por primera vez', mensaje = "Número de estudiantes de pregrado matriculados por primera vez", titulo = "Estudiantes de pregrado matriculados por primera vez");MAT_PVEZ_TABLA
MAT_PVEZ_SERIE <- series(datos = Consolidado, categoria = "MAT_PVEZ", colores = col, titulo = "Evolución del número de estudiantes de pregrado matriculados por primera vez",  eje = "Número de estudiantes (k: miles)");MAT_PVEZ_SERIE
MAT_PVEZ_ACTUAL <- torta(datos = Consolidado, variable = "MAT_PVEZ", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado por primera vez", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MAT_PVEZ_ACTUAL


# Modalidad de admisión ---

col <-   c( "#f15a24",  # naranja, Especial
            "#8cc63f" # verde, Regular
)

MOD_ADM_TABLA <- tabla(datos = Consolidado, categoria = "MOD_ADM", variable = 'Total estudiantes matriculados en pregrado según modalidad de admisión', mensaje = "Número de matriculados en pregrado según modalidad de admisión", titulo = "Matriculados en pregrado según modalidad de admisión");MOD_ADM_TABLA
MOD_ADM_SERIE <- series(datos = Consolidado, categoria = "MOD_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por modalidad de admisión", eje = "Número de estudiantes (k: miles)");MOD_ADM_SERIE
MOD_ADM_ACTUAL <- torta(datos = Consolidado, variable = "MOD_ADM", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado según modalidad de admisión", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MOD_ADM_ACTUAL


# tipo de admisión ---

col <-   c( "#f15a24", # naranja, PAES
            "#6d6666", # gris, PEAA
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f" # verde, Regular
)


TIPO_ADM_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_ADM", variable = 'Total estudiantes matriculados en pregrado por programa de admisión', mensaje = "Número de matriculados en pregrado por programa de admisión",titulo = "Matriculados en pregrado por programa de admisión");TIPO_ADM_TABLA
TIPO_ADM_SERIE <- series(datos = Consolidado, categoria = "TIPO_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por programa de admisión", eje = "Número de estudiantes (k: miles)");TIPO_ADM_SERIE
TIPO_ADM_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_ADM", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado por programa de admisión", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_ADM_ACTUAL


# PAES ---

col <-   c( "#0071bc", # azul vivo, comunidades indigenas
            "#9e9ac8",  # Morado claro, De La Paz
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666", # gris, mejores bachilleres municipios pobres
            "#f15a24", # naranja, población afro
            "#8cc63f") # verde, victimas del conflicto


PAES_TABLA <- tabla(datos = Consolidado, categoria = "PAES", variable = 'Total estudiantes matriculados en pregrado programa PAES', mensaje = "Número de matriculados en pregrado del programa PAES", titulo = "Matriculados en pregrado del programa PAES");PAES_TABLA
PAES_SERIE <- series(datos = Consolidado, categoria = "PAES", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado del programa PAES", eje = "Número de estudiantes (k: miles)");PAES_SERIE
PAES_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "PAES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado del programa PAES", eje = "Número de estudiantes (k: miles)"); PAES_ACTUAL


# PEAMA ---

col <-   c( "#29abe2", # azul claro, Amazonía
            "#c1272d", # Rojo, Caribe
            "#f15a24", # naranja, Medellín - Sinifaná
            "#fbb03b", # amarillo, Orinoquia
            "#8cc63f", # verde, Bogotá - Sumapaz
            "#0071bc", # azul vivo, Manizales
            "#6d6666" # gris, Tumaco
)


PEAMA_TABLA <- tabla(datos = Consolidado, categoria = "PEAMA", variable = 'Total estudiantes matriculados en pregrado programa PEAMA', mensaje = "Número de estudiantes matriculados en pregrado programa PEAMA", titulo = "Matriculados en pregrado programa PEAMA");PEAMA_TABLA
PEAMA_SERIE <- series(datos = Consolidado, categoria = "PEAMA", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado del programa PEAMA", eje = "Número de matriculados");PEAMA_SERIE
PEAMA_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "PEAMA", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado del programa PEAMA", eje = "Número de estudiantes"); PEAMA_ACTUAL


# Etapa actual PEAMA ---


col <-   c( "#f15a24", # naranja, Etapa de movilidad
            "#8cc63f") # verde, Etapa inicial


MOV_PEAMA_TABLA <- tabla(datos = Consolidado, categoria = "MOV_PEAMA", variable = 'Total estudiantes PEAMA según etapa de formación', mensaje = "Número de estudiantes de pregrado del programa PEAMA según etapa", titulo = "Estudiantes de pregrado PEAMA por etapa");MOV_PEAMA_TABLA
MOV_PEAMA_SERIE <- series(datos = Consolidado, categoria = "MOV_PEAMA", colores = col, titulo = "Evolución del número de estudiantes de pregrado del programa PEAMA según etapa de formación", eje = "Número de estudiantes (k: miles)");MOV_PEAMA_SERIE
MOV_PEAMA_ACTUAL <- torta(datos = Consolidado, variable = "MOV_PEAMA", colores = col, titulo = "Distribución de estudiantes del programa PEAMA según etapa de formación", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MOV_PEAMA_ACTUAL



# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales    

################ 1. Tabla

AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados en pregrado por áreas del conocimiento SNIES', mensaje = "Total de estudiantes matriculados en pregrado por áreas del conocimiento SNIES", titulo = "Sede estudiantes matriculados pregrado por áreas del conocimiento SNIES");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por áreas del conocimiento SNIES", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por áreas del conocimiento SNIES", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Nal/Pregrado", "Serie.html")
Salvar(SEDE_NOMBRE_MAT_TABLA, "G_Matriculados/Nal/Pregrado", "T_sede.html")
Salvar(SEDE_NOMBRE_MAT_SERIE, "G_Matriculados/Nal/Pregrado", "S_sede.html")
Salvar(SEDE_NOMBRE_MAT_ACTUAL, "G_Matriculados/Nal/Pregrado", "A_sede.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Nal/Pregrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Nal/Pregrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Nal/Pregrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Nal/Pregrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Nal/Pregrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Nal/Pregrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Nal/Pregrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Nal/Pregrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Nal/Pregrado", "A_edad.html")
Salvar(ESTRATO_TABLA, "G_Matriculados/Nal/Pregrado", "T_estrato.html")
Salvar(ESTRATO_SERIE, "G_Matriculados/Nal/Pregrado", "S_estrato.html")
Salvar(ESTRATO_ACTUAL, "G_Matriculados/Nal/Pregrado", "A_estrato.html")
Salvar(TIPO_COL_TABLA, "G_Matriculados/Nal/Pregrado", "T_colegio.html")
Salvar(TIPO_COL_SERIE, "G_Matriculados/Nal/Pregrado", "S_colegio.html")
Salvar(TIPO_COL_ACTUAL, "G_Matriculados/Nal/Pregrado", "A_colegio.html")
Salvar(PBM_TABLA, "G_Matriculados/Nal/Pregrado", "T_pbm.html")
Salvar(PBM_SERIE, "G_Matriculados/Nal/Pregrado", "S_pbm.html")
Salvar(PBM_ACTUAL, "G_Matriculados/Nal/Pregrado", "A_pbm.html")
Salvar(MAT_PVEZ_TABLA, "G_Matriculados/Nal/Pregrado", "T_mpv.html")
Salvar(MAT_PVEZ_SERIE, "G_Matriculados/Nal/Pregrado", "S_mpv.html")
Salvar(MAT_PVEZ_ACTUAL, "G_Matriculados/Nal/Pregrado", "A_mpv.html")
Salvar(MOD_ADM_TABLA, "G_Matriculados/Nal/Pregrado", "T_modalidad.html")
Salvar(MOD_ADM_SERIE, "G_Matriculados/Nal/Pregrado", "S_modalidad.html")
Salvar(MOD_ADM_ACTUAL, "G_Matriculados/Nal/Pregrado", "A_modalidad.html")
Salvar(TIPO_ADM_TABLA, "G_Matriculados/Nal/Pregrado", "T_tipo.html")
Salvar(TIPO_ADM_SERIE, "G_Matriculados/Nal/Pregrado", "S_tipo.html")
Salvar(TIPO_ADM_ACTUAL, "G_Matriculados/Nal/Pregrado", "A_tipo.html")
Salvar(PAES_TABLA, "G_Matriculados/Nal/Pregrado", "T_paes.html")
Salvar(PAES_SERIE, "G_Matriculados/Nal/Pregrado", "S_paes.html")
Salvar(PAES_ACTUAL, "G_Matriculados/Nal/Pregrado", "A_paes.html")
Salvar(PEAMA_TABLA, "G_Matriculados/Nal/Pregrado", "T_peama.html")
Salvar(PEAMA_SERIE, "G_Matriculados/Nal/Pregrado", "S_peama.html")
Salvar(PEAMA_ACTUAL, "G_Matriculados/Nal/Pregrado", "A_peama.html")
Salvar(MOV_PEAMA_TABLA, "G_Matriculados/Nal/Pregrado", "T_etapa.html")
Salvar(MOV_PEAMA_SERIE, "G_Matriculados/Nal/Pregrado", "S_etapa.html")
Salvar(MOV_PEAMA_ACTUAL, "G_Matriculados/Nal/Pregrado", "A_etapa.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Nal/Pregrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Nal/Pregrado", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Nal/Pregrado", "A_snies.html")



# Mt_Pre1101 ----


# Base de datos agregada 

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPre1101") %>% select(-(Nivel))


# Evolución histórica ---


col <-   c("#8cc63f") # Verde, Total sede Bogotá

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados en pregrado - sede Bogotá ", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Facultad ---

col <-   c( "#9e9ac8",  # Morado claro, Enfermería
            "#0071bc", # azul vivo, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias económicas
            "#f15a24", # naranja, Ciencias humanas
            "#fbb03b", # amarillo, Derecho, ciencias políticas y sociales
            "#93278f", # Morado, Artes 
            "#8cc63f", # verde, Ingeniería
            "#bdbdbd",  # Gris claro, Medicina 
            "#c1272d", # rojo, Medicina veterianaria y zootecnia
            "#99d8c9"  # Agua Marina, Odontología 
            
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados en pregrado por facultad - sede Bogotá', mensaje = "Total de estudiantes matriculados en pregrado por facultad - sede Bogotá", titulo = "Facultad estudiantes matriculados en pregrado - sede Bogotá");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por facultad - sede Bogotá", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por facultad - sede Bogotá", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información

NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados en pregrado según nacionalidad - sede Bogotá', mensaje = "Número de estudiantes matriculados en pregrado por nacionalidad - sede Bogotá", titulo = "Matriculados según nacionalidad - sede Bogotá");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado según nacionalidad - sede Bogotá", eje = "Número de estudiantes (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados en pregrado según la nacionalidad - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL

# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados en pregrado por sexo - sede Bogotá', mensaje = "Número de estudiantes de pregrado por sexo - sede Bogotá", titulo = "Estudiantes de pregrado por sexo - sede Bogotá");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por sexo - sede Bogotá", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado por sexo - sede Bogotá", etiqueta = "Número de estudiantes",ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados en pregrado por grupos de edad - sede Bogotá', mensaje = "Número de matriculados en pregrado por grupos de edad - sede Bogotá", titulo = "Matriculados en pregrado por grupos de edad - sede Bogotá");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por grupos de edad - sede Bogotá", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por grupos de edad - sede Bogotá", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL


# Estrato ---

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)


ESTRATO_TABLA <- tabla(datos = Consolidado, categoria = "ESTRATO", variable = 'Total de estudiantes matriculados en pregrado por estrato socioeconómico - sede Bogotá', mensaje = "Número de matriculados en pregrado por estrato - sede Bogotá", titulo = "Matriculados en pregrado por estrato - sede Bogotá");ESTRATO_TABLA
ESTRATO_SERIE <- series(datos = Consolidado, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por estrato socioeconómico - sede Bogotá", eje = "Número de estudiantes (k: miles)");ESTRATO_SERIE
ESTRATO_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por estrato - sede Bogotá", eje = "Número de estudiantes (k: miles)"); ESTRATO_ACTUAL


# Colegio ---

col <-   c( "#8cc63f", # verde, Oficial
            "#0071bc", # azul vivo, Otros 
            "#f15a24", # naranja, Privado 
            "#6d6666" # gris, Sin información
)


################ 1. Tabla

TIPO_COL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_COL", variable = 'Total estudiantes matriculados en pregrado según la naturaleza del colegio - sede Bogotá', mensaje = "Número de matriculados en pregrado según naturaleza del colegio - sede Bogotá", titulo = "Matriculados en pregrado según naturaleza del colegio - sede Bogotá");TIPO_COL_TABLA
TIPO_COL_SERIE <- series(datos = Consolidado, categoria = "TIPO_COL", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por naturaleza del colegio - sede Bogotá", eje = "Número de estudiantes (k: miles)");TIPO_COL_SERIE
TIPO_COL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_COL", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado según naturaleza del colegio - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_COL_ACTUAL


# PBM ---

col <-   c( "#8cc63f", # verde, 11 o menos
            "#6d6666", # gris, 12 a 17
            "#f15a24", # naranja, 18 a 50 
            "#0071bc", # azul vivo, 51 a 100
            "#93278f" # Morado, Sin información
)


PBM_TABLA <- tabla(datos = Consolidado, categoria = "PBM", variable = 'Total estudiantes matriculados en pregrado según Puntaje Básico Matrícula (PBM) - sede  Bogotá', mensaje = "Número de matriculados en pregrado según Puntaje Básico Matrícula (PBM) - sede Bogotá", titulo = "Matriculados en pregrado según Puntaje Básico Matrícula (PBM) - sede Bogotá");PBM_TABLA
PBM_SERIE <- series(datos = Consolidado, categoria = "PBM", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por grupos en Puntaje Básico Matrícula (PBM) - sede Bogotá", eje = "Número de estudiantes (k: miles)");PBM_SERIE
PBM_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "PBM", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por grupos de Puntaje Básico Matrícula (PBM) - sede Bogotá", eje = "Número de estudiantes (k: miles)"); PBM_ACTUAL


# Matriculados primera vez ---


col <-   c( "#0071bc", # azul vivo, 
            "#8cc63f") # verde, 

MAT_PVEZ_TABLA <- tabla(datos = Consolidado, categoria = "MAT_PVEZ", variable = 'Total estudiantes de pregrado matriculados por primera vez - sede Bogotá', mensaje = "Número de estudiantes de pregrado matriculados por primera vez - sede Bogotá", titulo = "Estudiantes de pregrado matriculados por primera vez - sede Bogotá");MAT_PVEZ_TABLA
MAT_PVEZ_SERIE <- series(datos = Consolidado, categoria = "MAT_PVEZ", colores = col, titulo = "Evolución del número de estudiantes de pregrado matriculados por primera vez - sede Bogotá",  eje = "Número de estudiantes (k: miles)");MAT_PVEZ_SERIE
MAT_PVEZ_ACTUAL <- torta(datos = Consolidado, variable = "MAT_PVEZ", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado por primera vez - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MAT_PVEZ_ACTUAL


# Modalidad de admisión ---

col <-   c( "#f15a24",  # naranja, Especial
            "#8cc63f" # verde, Regular
)

MOD_ADM_TABLA <- tabla(datos = Consolidado, categoria = "MOD_ADM", variable = 'Total estudiantes matriculados en pregrado según modalidad de admisión - sede Bogotá', mensaje = "Número de matriculados en pregrado según modalidad de admisión - sede Bogotá", titulo = "Matriculados en pregrado según modalidad de admisión - sede Bogotá");MOD_ADM_TABLA
MOD_ADM_SERIE <- series(datos = Consolidado, categoria = "MOD_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por modalidad de admisión - sede Bogotá", eje = "Número de estudiantes (k: miles)");MOD_ADM_SERIE
MOD_ADM_ACTUAL <- torta(datos = Consolidado, variable = "MOD_ADM", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado según modalidad de admisión - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MOD_ADM_ACTUAL


# tipo de admisión ---

col <-   c( "#f15a24", # naranja, PAES
            "#6d6666", # gris, PEAA
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f" # verde, Regular
)


TIPO_ADM_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_ADM", variable = 'Total estudiantes matriculados en pregrado por programa de admisión - sede Bogotá', mensaje = "Número de matriculados en pregrado por programa de admisión - sede Bogotá",titulo = "Matriculados en pregrado por programa de admisión - sede Bogotá");TIPO_ADM_TABLA
TIPO_ADM_SERIE <- series(datos = Consolidado, categoria = "TIPO_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por programa de admisión - sede Bogotá", eje = "Número de estudiantes (k: miles)");TIPO_ADM_SERIE
TIPO_ADM_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_ADM", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado por programa de admisión - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_ADM_ACTUAL


# PAES ---


col <-   c( "#0071bc", # azul vivo, comunidades indigenas
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666",  # gris, mejores bachilleres municipios pobres 
            "#f15a24", # naranja, población afro
            "#8cc63f"  # verde, victimas del conflicto
)


PAES_TABLA <- tabla(datos = Consolidado, categoria = "PAES", variable = 'Total estudiantes matriculados en pregrado programa PAES - sede Bogotá', mensaje = "Número de matriculados en pregrado del programa PAES - sede Bogotá", titulo = "Matriculados en pregrado del programa PAES - sede Bogotá");PAES_TABLA
PAES_SERIE <- series(datos = Consolidado, categoria = "PAES", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado del programa PAES - sede Bogotá", eje = "Número de estudiantes (k: miles)");PAES_SERIE
PAES_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "PAES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado del programa PAES - sede Bogotá", eje = "Número de estudiantes (k: miles)"); PAES_ACTUAL


# PEAMA ---

col <-   c( "#29abe2", # azul claro, Amazonía
            "#c1272d", # Rojo, Caribe
            "#fbb03b", # amarillo, Orinoquia
            "#8cc63f", # verde, Bogotá - Sumapaz
            "#6d6666"  # gris, Tumaco
)


PEAMA_TABLA <- tabla(datos = Consolidado, categoria = "PEAMA", variable = 'Total estudiantes matriculados en pregrado programa PEAMA - sede Bogotá', mensaje = "Número de estudiantes matriculados en pregrado programa PEAMA - sede Bogotá", titulo = "Matriculados en pregrado programa PEAMA - sede Bogotá");PEAMA_TABLA
PEAMA_SERIE <- series(datos = Consolidado, categoria = "PEAMA", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado del programa PEAMA - sede Bogotá", eje = "Número de matriculados");PEAMA_SERIE
PEAMA_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "PEAMA", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado del programa PEAMA - sede Bogotá", eje = "Número de estudiantes"); PEAMA_ACTUAL



# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales    

################ 1. Tabla

AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados en pregrado por áreas del conocimiento SNIES - sede Bogotá', mensaje = "Total de estudiantes matriculados en pregrado por áreas del conocimiento SNIES - sede Bogotá", titulo = "Sede estudiantes matriculados pregrado por áreas del conocimiento SNIES - sede Bogotá");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Bog/Pregrado", "Serie.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Bog/Pregrado", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Bog/Pregrado", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Bog/Pregrado", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Bog/Pregrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Bog/Pregrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Bog/Pregrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Bog/Pregrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Bog/Pregrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Bog/Pregrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Bog/Pregrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Bog/Pregrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Bog/Pregrado", "A_edad.html")
Salvar(ESTRATO_TABLA, "G_Matriculados/Bog/Pregrado", "T_estrato.html")
Salvar(ESTRATO_SERIE, "G_Matriculados/Bog/Pregrado", "S_estrato.html")
Salvar(ESTRATO_ACTUAL, "G_Matriculados/Bog/Pregrado", "A_estrato.html")
Salvar(TIPO_COL_TABLA, "G_Matriculados/Bog/Pregrado", "T_colegio.html")
Salvar(TIPO_COL_SERIE, "G_Matriculados/Bog/Pregrado", "S_colegio.html")
Salvar(TIPO_COL_ACTUAL, "G_Matriculados/Bog/Pregrado", "A_colegio.html")
Salvar(PBM_TABLA, "G_Matriculados/Bog/Pregrado", "T_pbm.html")
Salvar(PBM_SERIE, "G_Matriculados/Bog/Pregrado", "S_pbm.html")
Salvar(PBM_ACTUAL, "G_Matriculados/Bog/Pregrado", "A_pbm.html")
Salvar(MAT_PVEZ_TABLA, "G_Matriculados/Bog/Pregrado", "T_mpv.html")
Salvar(MAT_PVEZ_SERIE, "G_Matriculados/Bog/Pregrado", "S_mpv.html")
Salvar(MAT_PVEZ_ACTUAL, "G_Matriculados/Bog/Pregrado", "A_mpv.html")
Salvar(MOD_ADM_TABLA, "G_Matriculados/Bog/Pregrado", "T_modalidad.html")
Salvar(MOD_ADM_SERIE, "G_Matriculados/Bog/Pregrado", "S_modalidad.html")
Salvar(MOD_ADM_ACTUAL, "G_Matriculados/Bog/Pregrado", "A_modalidad.html")
Salvar(TIPO_ADM_TABLA, "G_Matriculados/Bog/Pregrado", "T_tipo.html")
Salvar(TIPO_ADM_SERIE, "G_Matriculados/Bog/Pregrado", "S_tipo.html")
Salvar(TIPO_ADM_ACTUAL, "G_Matriculados/Bog/Pregrado", "A_tipo.html")
Salvar(PAES_TABLA, "G_Matriculados/Bog/Pregrado", "T_paes.html")
Salvar(PAES_SERIE, "G_Matriculados/Bog/Pregrado", "S_paes.html")
Salvar(PAES_ACTUAL, "G_Matriculados/Bog/Pregrado", "A_paes.html")
Salvar(PEAMA_TABLA, "G_Matriculados/Bog/Pregrado", "T_peama.html")
Salvar(PEAMA_SERIE, "G_Matriculados/Bog/Pregrado", "S_peama.html")
Salvar(PEAMA_ACTUAL, "G_Matriculados/Bog/Pregrado", "A_peama.html")
Salvar(MOV_PEAMA_TABLA, "G_Matriculados/Bog/Pregrado", "T_etapa.html")
Salvar(MOV_PEAMA_SERIE, "G_Matriculados/Bog/Pregrado", "S_etapa.html")
Salvar(MOV_PEAMA_ACTUAL, "G_Matriculados/Bog/Pregrado", "A_etapa.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Bog/Pregrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Bog/Pregrado", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Bog/Pregrado", "A_snies.html")




# Mt_Pre1102 ----


# Base de datos agregada 

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPre1102") %>% select(-(Nivel))


# Evolución histórica ---


col <-   c("#f15a24") # Naranja, Total sede Medellín

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados en pregrado - sede Medellín ", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Facultad ---

col <-   c( "#9e9ac8",  # Morado claro, Arquitectura
            "#f15a24", # naranja, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias humanas y económicas 
            "#8cc63f" # verde, Minas
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados en pregrado por facultad - sede Medellín', mensaje = "Total de estudiantes matriculados en pregrado por facultad - sede Medellín", titulo = "Facultad estudiantes matriculados en pregrado - sede Medellín");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por facultad - sede Medellín", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por facultad - sede Medellín", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información

NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados en pregrado según nacionalidad - sede Medellín', mensaje = "Número de estudiantes matriculados en pregrado por nacionalidad - sede Medellín", titulo = "Matriculados según nacionalidad - sede Medellín");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado según nacionalidad - sede Medellín", eje = "Número de estudiantes (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados en pregrado según la nacionalidad - sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL

# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados en pregrado por sexo - sede Medellín', mensaje = "Número de estudiantes de pregrado por sexo - sede Medellín", titulo = "Estudiantes de pregrado por sexo - sede Medellín");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por sexo - sede Medellín", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado por sexo - sede Medellín", etiqueta = "Número de estudiantes",ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados en pregrado por grupos de edad - sede Medellín', mensaje = "Número de matriculados en pregrado por grupos de edad - sede Medellín", titulo = "Matriculados en pregrado por grupos de edad - sede Medellín");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por grupos de edad - sede Medellín", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por grupos de edad - sede Medellín", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL


# Estrato ---

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)


ESTRATO_TABLA <- tabla(datos = Consolidado, categoria = "ESTRATO", variable = 'Total de estudiantes matriculados en pregrado por estrato socioeconómico - sede Medellín', mensaje = "Número de matriculados en pregrado por estrato - sede Medellín", titulo = "Matriculados en pregrado por estrato - sede Medellín");ESTRATO_TABLA
ESTRATO_SERIE <- series(datos = Consolidado, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por estrato socioeconómico - sede Medellín", eje = "Número de estudiantes (k: miles)");ESTRATO_SERIE
ESTRATO_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por estrato - sede Medellín", eje = "Número de estudiantes (k: miles)"); ESTRATO_ACTUAL


# Colegio ---

col <-   c( "#8cc63f", # verde, Oficial
            "#0071bc", # azul vivo, Otros 
            "#f15a24", # naranja, Privado 
            "#6d6666" # gris, Sin información
)


################ 1. Tabla

TIPO_COL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_COL", variable = 'Total estudiantes matriculados en pregrado según la naturaleza del colegio - sede Medellín', mensaje = "Número de matriculados en pregrado según naturaleza del colegio - sede Medellín", titulo = "Matriculados en pregrado según naturaleza del colegio - sede Medellín");TIPO_COL_TABLA
TIPO_COL_SERIE <- series(datos = Consolidado, categoria = "TIPO_COL", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por naturaleza del colegio - sede Medellín", eje = "Número de estudiantes (k: miles)");TIPO_COL_SERIE
TIPO_COL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_COL", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado según naturaleza del colegio - sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_COL_ACTUAL


# PBM ---

col <-   c( "#8cc63f", # verde, 11 o menos
            "#6d6666", # gris, 12 a 17
            "#f15a24", # naranja, 18 a 50 
            "#0071bc", # azul vivo, 51 a 100
            "#93278f" # Morado, Sin información
)


PBM_TABLA <- tabla(datos = Consolidado, categoria = "PBM", variable = 'Total estudiantes matriculados en pregrado según Puntaje Básico Matrícula (PBM) - sede  Medellín', mensaje = "Número de matriculados en pregrado según Puntaje Básico Matrícula (PBM) - sede Medellín", titulo = "Matriculados en pregrado según Puntaje Básico Matrícula (PBM) - sede Medellín");PBM_TABLA
PBM_SERIE <- series(datos = Consolidado, categoria = "PBM", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por grupos en Puntaje Básico Matrícula (PBM) - sede Medellín", eje = "Número de estudiantes (k: miles)");PBM_SERIE
PBM_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "PBM", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por grupos de Puntaje Básico Matrícula (PBM) - sede Medellín", eje = "Número de estudiantes (k: miles)"); PBM_ACTUAL


# Matriculados primera vez ---


col <-   c( "#0071bc", # azul vivo, 
            "#8cc63f") # verde, 

MAT_PVEZ_TABLA <- tabla(datos = Consolidado, categoria = "MAT_PVEZ", variable = 'Total estudiantes de pregrado matriculados por primera vez - sede Medellín', mensaje = "Número de estudiantes de pregrado matriculados por primera vez - sede Medellín", titulo = "Estudiantes de pregrado matriculados por primera vez - sede Medellín");MAT_PVEZ_TABLA
MAT_PVEZ_SERIE <- series(datos = Consolidado, categoria = "MAT_PVEZ", colores = col, titulo = "Evolución del número de estudiantes de pregrado matriculados por primera vez - sede Medellín",  eje = "Número de estudiantes (k: miles)");MAT_PVEZ_SERIE
MAT_PVEZ_ACTUAL <- torta(datos = Consolidado, variable = "MAT_PVEZ", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado por primera vez - sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MAT_PVEZ_ACTUAL


# Modalidad de admisión ---

col <-   c( "#f15a24",  # naranja, Especial
            "#8cc63f" # verde, Regular
)

MOD_ADM_TABLA <- tabla(datos = Consolidado, categoria = "MOD_ADM", variable = 'Total estudiantes matriculados en pregrado según modalidad de admisión - sede Medellín', mensaje = "Número de matriculados en pregrado según modalidad de admisión - sede Medellín", titulo = "Matriculados en pregrado según modalidad de admisión - sede Medellín");MOD_ADM_TABLA
MOD_ADM_SERIE <- series(datos = Consolidado, categoria = "MOD_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por modalidad de admisión - sede Medellín", eje = "Número de estudiantes (k: miles)");MOD_ADM_SERIE
MOD_ADM_ACTUAL <- torta(datos = Consolidado, variable = "MOD_ADM", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado según modalidad de admisión - sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MOD_ADM_ACTUAL


# tipo de admisión ---

col <-   c( "#f15a24", # naranja, PAES
            "#6d6666", # gris, PEAA
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f" # verde, Regular
)


TIPO_ADM_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_ADM", variable = 'Total estudiantes matriculados en pregrado por programa de admisión - sede Medellín', mensaje = "Número de matriculados en pregrado por programa de admisión - sede Medellín",titulo = "Matriculados en pregrado por programa de admisión - sede Medellín");TIPO_ADM_TABLA
TIPO_ADM_SERIE <- series(datos = Consolidado, categoria = "TIPO_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por programa de admisión - sede Medellín", eje = "Número de estudiantes (k: miles)");TIPO_ADM_SERIE
TIPO_ADM_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_ADM", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado por programa de admisión - sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_ADM_ACTUAL


# PAES ---


col <-   c( "#0071bc", # azul vivo, comunidades indigenas
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666",  # gris, mejores bachilleres municipios pobres 
            "#f15a24", # naranja, población afro
            "#8cc63f"  # verde, victimas del conflicto
)


PAES_TABLA <- tabla(datos = Consolidado, categoria = "PAES", variable = 'Total estudiantes matriculados en pregrado programa PAES - sede Medellín', mensaje = "Número de matriculados en pregrado del programa PAES - sede Medellín", titulo = "Matriculados en pregrado del programa PAES - sede Medellín");PAES_TABLA
PAES_SERIE <- series(datos = Consolidado, categoria = "PAES", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado del programa PAES - sede Medellín", eje = "Número de estudiantes (k: miles)");PAES_SERIE
PAES_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "PAES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado del programa PAES - sede Medellín", eje = "Número de estudiantes (k: miles)"); PAES_ACTUAL


# PEAMA ---

col <-   c( "#29abe2", # azul claro, Amazonía
            "#c1272d", # Rojo, Caribe
            "#f15a24", # naranja, Medellín - Sinifaná
            "#fbb03b", # amarillo, Orinoquia
            "#6d6666" # gris, Tumaco
)


PEAMA_TABLA <- tabla(datos = Consolidado, categoria = "PEAMA", variable = 'Total estudiantes matriculados en pregrado programa PEAMA - sede Medellín', mensaje = "Número de estudiantes matriculados en pregrado programa PEAMA - sede Medellín", titulo = "Matriculados en pregrado programa PEAMA - sede Medellín");PEAMA_TABLA
PEAMA_SERIE <- series(datos = Consolidado, categoria = "PEAMA", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado del programa PEAMA - sede Medellín", eje = "Número de matriculados");PEAMA_SERIE
PEAMA_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "PEAMA", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado del programa PEAMA - sede Medellín", eje = "Número de estudiantes"); PEAMA_ACTUAL



# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales    

################ 1. Tabla

AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados en pregrado por áreas del conocimiento SNIES - sede Medellín', mensaje = "Total de estudiantes matriculados en pregrado por áreas del conocimiento SNIES - sede Medellín", titulo = "Sede estudiantes matriculados pregrado por áreas del conocimiento SNIES - sede Medellín");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por áreas del conocimiento SNIES - sede Medellín", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por áreas del conocimiento SNIES - sede Medellín", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Med/Pregrado", "Serie.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Med/Pregrado", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Med/Pregrado", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Med/Pregrado", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Med/Pregrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Med/Pregrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Med/Pregrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Med/Pregrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Med/Pregrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Med/Pregrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Med/Pregrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Med/Pregrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Med/Pregrado", "A_edad.html")
Salvar(ESTRATO_TABLA, "G_Matriculados/Med/Pregrado", "T_estrato.html")
Salvar(ESTRATO_SERIE, "G_Matriculados/Med/Pregrado", "S_estrato.html")
Salvar(ESTRATO_ACTUAL, "G_Matriculados/Med/Pregrado", "A_estrato.html")
Salvar(TIPO_COL_TABLA, "G_Matriculados/Med/Pregrado", "T_colegio.html")
Salvar(TIPO_COL_SERIE, "G_Matriculados/Med/Pregrado", "S_colegio.html")
Salvar(TIPO_COL_ACTUAL, "G_Matriculados/Med/Pregrado", "A_colegio.html")
Salvar(PBM_TABLA, "G_Matriculados/Med/Pregrado", "T_pbm.html")
Salvar(PBM_SERIE, "G_Matriculados/Med/Pregrado", "S_pbm.html")
Salvar(PBM_ACTUAL, "G_Matriculados/Med/Pregrado", "A_pbm.html")
Salvar(MAT_PVEZ_TABLA, "G_Matriculados/Med/Pregrado", "T_mpv.html")
Salvar(MAT_PVEZ_SERIE, "G_Matriculados/Med/Pregrado", "S_mpv.html")
Salvar(MAT_PVEZ_ACTUAL, "G_Matriculados/Med/Pregrado", "A_mpv.html")
Salvar(MOD_ADM_TABLA, "G_Matriculados/Med/Pregrado", "T_modalidad.html")
Salvar(MOD_ADM_SERIE, "G_Matriculados/Med/Pregrado", "S_modalidad.html")
Salvar(MOD_ADM_ACTUAL, "G_Matriculados/Med/Pregrado", "A_modalidad.html")
Salvar(TIPO_ADM_TABLA, "G_Matriculados/Med/Pregrado", "T_tipo.html")
Salvar(TIPO_ADM_SERIE, "G_Matriculados/Med/Pregrado", "S_tipo.html")
Salvar(TIPO_ADM_ACTUAL, "G_Matriculados/Med/Pregrado", "A_tipo.html")
Salvar(PAES_TABLA, "G_Matriculados/Med/Pregrado", "T_paes.html")
Salvar(PAES_SERIE, "G_Matriculados/Med/Pregrado", "S_paes.html")
Salvar(PAES_ACTUAL, "G_Matriculados/Med/Pregrado", "A_paes.html")
Salvar(PEAMA_TABLA, "G_Matriculados/Med/Pregrado", "T_peama.html")
Salvar(PEAMA_SERIE, "G_Matriculados/Med/Pregrado", "S_peama.html")
Salvar(PEAMA_ACTUAL, "G_Matriculados/Med/Pregrado", "A_peama.html")
Salvar(MOV_PEAMA_TABLA, "G_Matriculados/Med/Pregrado", "T_etapa.html")
Salvar(MOV_PEAMA_SERIE, "G_Matriculados/Med/Pregrado", "S_etapa.html")
Salvar(MOV_PEAMA_ACTUAL, "G_Matriculados/Med/Pregrado", "A_etapa.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Med/Pregrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Med/Pregrado", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Med/Pregrado", "A_snies.html")




# Mt_Pre1103 ----


# Base de datos agregada 

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPre1103") %>% select(-(Nivel))


# Evolución histórica  ---


col <-   c("#0071bc") # Azul vivo, Total sede Manizales


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados en pregrado - sede Manizales ", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Facultad ---

col <-   c( "#f15a24", # naranja, Administración
            "#0071bc", # azul vivo, Ciencias exactas y naturales
            "#8cc63f" # verde, Ingeniaría y arquitectura
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados en pregrado por facultad - sede Manizales', mensaje = "Total de estudiantes matriculados en pregrado por facultad - sede Manizales", titulo = "Facultad estudiantes matriculados en pregrado - sede Manizales");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por facultad - sede Manizales", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por facultad - sede Manizales", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información

NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados en pregrado según nacionalidad - sede Manizales', mensaje = "Número de estudiantes matriculados en pregrado por nacionalidad - sede Manizales", titulo = "Matriculados según nacionalidad - sede Manizales");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado según nacionalidad - sede Manizales", eje = "Número de estudiantes (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados en pregrado según la nacionalidad - sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL

# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados en pregrado por sexo - sede Manizales', mensaje = "Número de estudiantes de pregrado por sexo - sede Manizales", titulo = "Estudiantes de pregrado por sexo - sede Manizales");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por sexo - sede Manizales", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado por sexo - sede Manizales", etiqueta = "Número de estudiantes",ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados en pregrado por grupos de edad - sede Manizales', mensaje = "Número de matriculados en pregrado por grupos de edad - sede Manizales", titulo = "Matriculados en pregrado por grupos de edad - sede Manizales");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por grupos de edad - sede Manizales", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por grupos de edad - sede Manizales", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL


# Estrato ---

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)


ESTRATO_TABLA <- tabla(datos = Consolidado, categoria = "ESTRATO", variable = 'Total de estudiantes matriculados en pregrado por estrato socioeconómico - sede Manizales', mensaje = "Número de matriculados en pregrado por estrato - sede Manizales", titulo = "Matriculados en pregrado por estrato - sede Manizales");ESTRATO_TABLA
ESTRATO_SERIE <- series(datos = Consolidado, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por estrato socioeconómico - sede Manizales", eje = "Número de estudiantes (k: miles)");ESTRATO_SERIE
ESTRATO_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por estrato - sede Manizales", eje = "Número de estudiantes (k: miles)"); ESTRATO_ACTUAL


# Colegio ---

col <-   c( "#8cc63f", # verde, Oficial
            "#0071bc", # azul vivo, Otros 
            "#f15a24", # naranja, Privado 
            "#6d6666" # gris, Sin información
)


################ 1. Tabla

TIPO_COL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_COL", variable = 'Total estudiantes matriculados en pregrado según la naturaleza del colegio - sede Manizales', mensaje = "Número de matriculados en pregrado según naturaleza del colegio - sede Manizales", titulo = "Matriculados en pregrado según naturaleza del colegio - sede Manizales");TIPO_COL_TABLA
TIPO_COL_SERIE <- series(datos = Consolidado, categoria = "TIPO_COL", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por naturaleza del colegio - sede Manizales", eje = "Número de estudiantes (k: miles)");TIPO_COL_SERIE
TIPO_COL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_COL", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado según naturaleza del colegio - sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_COL_ACTUAL


# PBM ---

col <-   c( "#8cc63f", # verde, 11 o menos
            "#6d6666", # gris, 12 a 17
            "#f15a24", # naranja, 18 a 50 
            "#0071bc", # azul vivo, 51 a 100
            "#93278f" # Morado, Sin información
)


PBM_TABLA <- tabla(datos = Consolidado, categoria = "PBM", variable = 'Total estudiantes matriculados en pregrado según Puntaje Básico Matrícula (PBM) - sede  Manizales', mensaje = "Número de matriculados en pregrado según Puntaje Básico Matrícula (PBM) - sede Manizales", titulo = "Matriculados en pregrado según Puntaje Básico Matrícula (PBM) - sede Manizales");PBM_TABLA
PBM_SERIE <- series(datos = Consolidado, categoria = "PBM", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por grupos en Puntaje Básico Matrícula (PBM) - sede Manizales", eje = "Número de estudiantes (k: miles)");PBM_SERIE
PBM_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "PBM", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por grupos de Puntaje Básico Matrícula (PBM) - sede Manizales", eje = "Número de estudiantes (k: miles)"); PBM_ACTUAL


# Matriculados primera vez ---


col <-   c( "#0071bc", # azul vivo, 
            "#8cc63f") # verde, 

MAT_PVEZ_TABLA <- tabla(datos = Consolidado, categoria = "MAT_PVEZ", variable = 'Total estudiantes de pregrado matriculados por primera vez - sede Manizales', mensaje = "Número de estudiantes de pregrado matriculados por primera vez - sede Manizales", titulo = "Estudiantes de pregrado matriculados por primera vez - sede Manizales");MAT_PVEZ_TABLA
MAT_PVEZ_SERIE <- series(datos = Consolidado, categoria = "MAT_PVEZ", colores = col, titulo = "Evolución del número de estudiantes de pregrado matriculados por primera vez - sede Manizales",  eje = "Número de estudiantes (k: miles)");MAT_PVEZ_SERIE
MAT_PVEZ_ACTUAL <- torta(datos = Consolidado, variable = "MAT_PVEZ", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado por primera vez - sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MAT_PVEZ_ACTUAL


# Modalidad de admisión ---

col <-   c( "#f15a24",  # naranja, Especial
            "#8cc63f" # verde, Regular
)

MOD_ADM_TABLA <- tabla(datos = Consolidado, categoria = "MOD_ADM", variable = 'Total estudiantes matriculados en pregrado según modalidad de admisión - sede Manizales', mensaje = "Número de matriculados en pregrado según modalidad de admisión - sede Manizales", titulo = "Matriculados en pregrado según modalidad de admisión - sede Manizales");MOD_ADM_TABLA
MOD_ADM_SERIE <- series(datos = Consolidado, categoria = "MOD_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por modalidad de admisión - sede Manizales", eje = "Número de estudiantes (k: miles)");MOD_ADM_SERIE
MOD_ADM_ACTUAL <- torta(datos = Consolidado, variable = "MOD_ADM", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado según modalidad de admisión - sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MOD_ADM_ACTUAL


# tipo de admisión ---

col <-   c( "#f15a24", # naranja, PAES
            "#6d6666", # gris, PEAA
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f" # verde, Regular
)


TIPO_ADM_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_ADM", variable = 'Total estudiantes matriculados en pregrado por programa de admisión - sede Manizales', mensaje = "Número de matriculados en pregrado por programa de admisión - sede Manizales",titulo = "Matriculados en pregrado por programa de admisión - sede Manizales");TIPO_ADM_TABLA
TIPO_ADM_SERIE <- series(datos = Consolidado, categoria = "TIPO_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por programa de admisión - sede Manizales", eje = "Número de estudiantes (k: miles)");TIPO_ADM_SERIE
TIPO_ADM_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_ADM", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado por programa de admisión - sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_ADM_ACTUAL


# PAES ---


col <-   c( "#0071bc", # azul vivo, comunidades indigenas
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666",  # gris, mejores bachilleres municipios pobres 
            "#f15a24", # naranja, población afro
            "#8cc63f"  # verde, victimas del conflicto
)


PAES_TABLA <- tabla(datos = Consolidado, categoria = "PAES", variable = 'Total estudiantes matriculados en pregrado programa PAES - sede Manizales', mensaje = "Número de matriculados en pregrado del programa PAES - sede Manizales", titulo = "Matriculados en pregrado del programa PAES - sede Manizales");PAES_TABLA
PAES_SERIE <- series(datos = Consolidado, categoria = "PAES", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado del programa PAES - sede Manizales", eje = "Número de estudiantes (k: miles)");PAES_SERIE
PAES_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "PAES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado del programa PAES - sede Manizales", eje = "Número de estudiantes (k: miles)"); PAES_ACTUAL


# PEAMA ---

col <-   c( "#29abe2", # azul claro, Amazonía
            "#c1272d", # Rojo, Caribe
            "#fbb03b", # amarillo, Orinoquia
            "#0071bc", # azul vivo, Manizales
            "#6d6666" # gris, Tumaco
)


PEAMA_TABLA <- tabla(datos = Consolidado, categoria = "PEAMA", variable = 'Total estudiantes matriculados en pregrado programa PEAMA - sede Manizales', mensaje = "Número de estudiantes matriculados en pregrado programa PEAMA - sede Manizales", titulo = "Matriculados en pregrado programa PEAMA - sede Manizales");PEAMA_TABLA
PEAMA_SERIE <- series(datos = Consolidado, categoria = "PEAMA", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado del programa PEAMA - sede Manizales", eje = "Número de matriculados");PEAMA_SERIE
PEAMA_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "PEAMA", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado del programa PEAMA - sede Manizales", eje = "Número de estudiantes"); PEAMA_ACTUAL



# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales    

################ 1. Tabla

AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados en pregrado por áreas del conocimiento SNIES - sede Manizales', mensaje = "Total de estudiantes matriculados en pregrado por áreas del conocimiento SNIES - sede Manizales", titulo = "Sede estudiantes matriculados pregrado por áreas del conocimiento SNIES - sede Manizales");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por áreas del conocimiento SNIES - sede Manizales", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por áreas del conocimiento SNIES - sede Manizales", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Man/Pregrado", "Serie.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Man/Pregrado", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Man/Pregrado", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Man/Pregrado", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Man/Pregrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Man/Pregrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Man/Pregrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Man/Pregrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Man/Pregrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Man/Pregrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Man/Pregrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Man/Pregrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Man/Pregrado", "A_edad.html")
Salvar(ESTRATO_TABLA, "G_Matriculados/Man/Pregrado", "T_estrato.html")
Salvar(ESTRATO_SERIE, "G_Matriculados/Man/Pregrado", "S_estrato.html")
Salvar(ESTRATO_ACTUAL, "G_Matriculados/Man/Pregrado", "A_estrato.html")
Salvar(TIPO_COL_TABLA, "G_Matriculados/Man/Pregrado", "T_colegio.html")
Salvar(TIPO_COL_SERIE, "G_Matriculados/Man/Pregrado", "S_colegio.html")
Salvar(TIPO_COL_ACTUAL, "G_Matriculados/Man/Pregrado", "A_colegio.html")
Salvar(PBM_TABLA, "G_Matriculados/Man/Pregrado", "T_pbm.html")
Salvar(PBM_SERIE, "G_Matriculados/Man/Pregrado", "S_pbm.html")
Salvar(PBM_ACTUAL, "G_Matriculados/Man/Pregrado", "A_pbm.html")
Salvar(MAT_PVEZ_TABLA, "G_Matriculados/Man/Pregrado", "T_mpv.html")
Salvar(MAT_PVEZ_SERIE, "G_Matriculados/Man/Pregrado", "S_mpv.html")
Salvar(MAT_PVEZ_ACTUAL, "G_Matriculados/Man/Pregrado", "A_mpv.html")
Salvar(MOD_ADM_TABLA, "G_Matriculados/Man/Pregrado", "T_modalidad.html")
Salvar(MOD_ADM_SERIE, "G_Matriculados/Man/Pregrado", "S_modalidad.html")
Salvar(MOD_ADM_ACTUAL, "G_Matriculados/Man/Pregrado", "A_modalidad.html")
Salvar(TIPO_ADM_TABLA, "G_Matriculados/Man/Pregrado", "T_tipo.html")
Salvar(TIPO_ADM_SERIE, "G_Matriculados/Man/Pregrado", "S_tipo.html")
Salvar(TIPO_ADM_ACTUAL, "G_Matriculados/Man/Pregrado", "A_tipo.html")
Salvar(PAES_TABLA, "G_Matriculados/Man/Pregrado", "T_paes.html")
Salvar(PAES_SERIE, "G_Matriculados/Man/Pregrado", "S_paes.html")
Salvar(PAES_ACTUAL, "G_Matriculados/Man/Pregrado", "A_paes.html")
Salvar(PEAMA_TABLA, "G_Matriculados/Man/Pregrado", "T_peama.html")
Salvar(PEAMA_SERIE, "G_Matriculados/Man/Pregrado", "S_peama.html")
Salvar(PEAMA_ACTUAL, "G_Matriculados/Man/Pregrado", "A_peama.html")
Salvar(MOV_PEAMA_TABLA, "G_Matriculados/Man/Pregrado", "T_etapa.html")
Salvar(MOV_PEAMA_SERIE, "G_Matriculados/Man/Pregrado", "S_etapa.html")
Salvar(MOV_PEAMA_ACTUAL, "G_Matriculados/Man/Pregrado", "A_etapa.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Man/Pregrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Man/Pregrado", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Man/Pregrado", "A_snies.html")


# Mt_Pre1104 ----


# Base de datos agregada 

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPre1104") %>% select(-(Nivel))


# Evolución histórica  ---


col <-   c("#93278f") # Morado, Total sede Palmira


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados en pregrado - sede Palmira ", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Facultad ---

col <-   c( "#0071bc", # azul vivo, Ciencias agropecuarias 
            "#8cc63f" # verde, Ingeniería y Administración
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados en pregrado por facultad - sede Palmira', mensaje = "Total de estudiantes matriculados en pregrado por facultad - sede Palmira", titulo = "Facultad estudiantes matriculados en pregrado - sede Palmira");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por facultad - sede Palmira", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por facultad - sede Palmira", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información

NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados en pregrado según nacionalidad - sede Palmira', mensaje = "Número de estudiantes matriculados en pregrado por nacionalidad - sede Palmira", titulo = "Matriculados según nacionalidad - sede Palmira");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado según nacionalidad - sede Palmira", eje = "Número de estudiantes (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados en pregrado según la nacionalidad - sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL

# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados en pregrado por sexo - sede Palmira', mensaje = "Número de estudiantes de pregrado por sexo - sede Palmira", titulo = "Estudiantes de pregrado por sexo - sede Palmira");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por sexo - sede Palmira", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado por sexo - sede Palmira", etiqueta = "Número de estudiantes",ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados en pregrado por grupos de edad - sede Palmira', mensaje = "Número de matriculados en pregrado por grupos de edad - sede Palmira", titulo = "Matriculados en pregrado por grupos de edad - sede Palmira");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por grupos de edad - sede Palmira", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por grupos de edad - sede Palmira", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL


# Estrato ---

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)


ESTRATO_TABLA <- tabla(datos = Consolidado, categoria = "ESTRATO", variable = 'Total de estudiantes matriculados en pregrado por estrato socioeconómico - sede Palmira', mensaje = "Número de matriculados en pregrado por estrato - sede Palmira", titulo = "Matriculados en pregrado por estrato - sede Palmira");ESTRATO_TABLA
ESTRATO_SERIE <- series(datos = Consolidado, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por estrato socioeconómico - sede Palmira", eje = "Número de estudiantes (k: miles)");ESTRATO_SERIE
ESTRATO_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por estrato - sede Palmira", eje = "Número de estudiantes (k: miles)"); ESTRATO_ACTUAL


# Colegio ---

col <-   c( "#8cc63f", # verde, Oficial
            "#0071bc", # azul vivo, Otros 
            "#f15a24", # naranja, Privado 
            "#6d6666" # gris, Sin información
)


################ 1. Tabla

TIPO_COL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_COL", variable = 'Total estudiantes matriculados en pregrado según la naturaleza del colegio - sede Palmira', mensaje = "Número de matriculados en pregrado según naturaleza del colegio - sede Palmira", titulo = "Matriculados en pregrado según naturaleza del colegio - sede Palmira");TIPO_COL_TABLA
TIPO_COL_SERIE <- series(datos = Consolidado, categoria = "TIPO_COL", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por naturaleza del colegio - sede Palmira", eje = "Número de estudiantes (k: miles)");TIPO_COL_SERIE
TIPO_COL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_COL", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado según naturaleza del colegio - sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_COL_ACTUAL


# PBM ---

col <-   c( "#8cc63f", # verde, 11 o menos
            "#6d6666", # gris, 12 a 17
            "#f15a24", # naranja, 18 a 50 
            "#0071bc", # azul vivo, 51 a 100
            "#93278f" # Morado, Sin información
)


PBM_TABLA <- tabla(datos = Consolidado, categoria = "PBM", variable = 'Total estudiantes matriculados en pregrado según Puntaje Básico Matrícula (PBM) - sede  Palmira', mensaje = "Número de matriculados en pregrado según Puntaje Básico Matrícula (PBM) - sede Palmira", titulo = "Matriculados en pregrado según Puntaje Básico Matrícula (PBM) - sede Palmira");PBM_TABLA
PBM_SERIE <- series(datos = Consolidado, categoria = "PBM", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por grupos en Puntaje Básico Matrícula (PBM) - sede Palmira", eje = "Número de estudiantes (k: miles)");PBM_SERIE
PBM_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "PBM", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por grupos de Puntaje Básico Matrícula (PBM) - sede Palmira", eje = "Número de estudiantes (k: miles)"); PBM_ACTUAL


# Matriculados primera vez ---


col <-   c( "#0071bc", # azul vivo, 
            "#8cc63f") # verde, 

MAT_PVEZ_TABLA <- tabla(datos = Consolidado, categoria = "MAT_PVEZ", variable = 'Total estudiantes de pregrado matriculados por primera vez - sede Palmira', mensaje = "Número de estudiantes de pregrado matriculados por primera vez - sede Palmira", titulo = "Estudiantes de pregrado matriculados por primera vez - sede Palmira");MAT_PVEZ_TABLA
MAT_PVEZ_SERIE <- series(datos = Consolidado, categoria = "MAT_PVEZ", colores = col, titulo = "Evolución del número de estudiantes de pregrado matriculados por primera vez - sede Palmira",  eje = "Número de estudiantes (k: miles)");MAT_PVEZ_SERIE
MAT_PVEZ_ACTUAL <- torta(datos = Consolidado, variable = "MAT_PVEZ", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado por primera vez - sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MAT_PVEZ_ACTUAL


# Modalidad de admisión ---

col <-   c( "#f15a24",  # naranja, Especial
            "#8cc63f" # verde, Regular
)

MOD_ADM_TABLA <- tabla(datos = Consolidado, categoria = "MOD_ADM", variable = 'Total estudiantes matriculados en pregrado según modalidad de admisión - sede Palmira', mensaje = "Número de matriculados en pregrado según modalidad de admisión - sede Palmira", titulo = "Matriculados en pregrado según modalidad de admisión - sede Palmira");MOD_ADM_TABLA
MOD_ADM_SERIE <- series(datos = Consolidado, categoria = "MOD_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por modalidad de admisión - sede Palmira", eje = "Número de estudiantes (k: miles)");MOD_ADM_SERIE
MOD_ADM_ACTUAL <- torta(datos = Consolidado, variable = "MOD_ADM", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado según modalidad de admisión - sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MOD_ADM_ACTUAL


# tipo de admisión ---

col <-   c( "#f15a24", # naranja, PAES
            "#6d6666", # gris, PEAA
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f" # verde, Regular
)


TIPO_ADM_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_ADM", variable = 'Total estudiantes matriculados en pregrado por programa de admisión - sede Palmira', mensaje = "Número de matriculados en pregrado por programa de admisión - sede Palmira",titulo = "Matriculados en pregrado por programa de admisión - sede Palmira");TIPO_ADM_TABLA
TIPO_ADM_SERIE <- series(datos = Consolidado, categoria = "TIPO_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por programa de admisión - sede Palmira", eje = "Número de estudiantes (k: miles)");TIPO_ADM_SERIE
TIPO_ADM_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_ADM", colores = col, titulo = "Distribución de estudiantes matriculados en pregrado por programa de admisión - sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_ADM_ACTUAL


# PAES ---


col <-   c( "#0071bc", # azul vivo, comunidades indigenas
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666",  # gris, mejores bachilleres municipios pobres 
            "#f15a24", # naranja, población afro
            "#8cc63f"  # verde, victimas del conflicto
)


PAES_TABLA <- tabla(datos = Consolidado, categoria = "PAES", variable = 'Total estudiantes matriculados en pregrado programa PAES - sede Palmira', mensaje = "Número de matriculados en pregrado del programa PAES - sede Palmira", titulo = "Matriculados en pregrado del programa PAES - sede Palmira");PAES_TABLA
PAES_SERIE <- series(datos = Consolidado, categoria = "PAES", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado del programa PAES - sede Palmira", eje = "Número de estudiantes (k: miles)");PAES_SERIE
PAES_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "PAES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado del programa PAES - sede Palmira", eje = "Número de estudiantes (k: miles)"); PAES_ACTUAL


# PEAMA ---

col <-   c( "#29abe2", # azul claro, Amazonía
            "#c1272d", # Rojo, Caribe
            "#fbb03b", # amarillo, Orinoquia
            "#6d6666" # gris, Tumaco
)


PEAMA_TABLA <- tabla(datos = Consolidado, categoria = "PEAMA", variable = 'Total estudiantes matriculados en pregrado programa PEAMA - sede Palmira', mensaje = "Número de estudiantes matriculados en pregrado programa PEAMA - sede Palmira", titulo = "Matriculados en pregrado programa PEAMA - sede Palmira");PEAMA_TABLA
PEAMA_SERIE <- series(datos = Consolidado, categoria = "PEAMA", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado del programa PEAMA - sede Palmira", eje = "Número de matriculados");PEAMA_SERIE
PEAMA_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "PEAMA", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado del programa PEAMA - sede Palmira", eje = "Número de estudiantes"); PEAMA_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales    

AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados en pregrado por áreas del conocimiento SNIES - sede Palmira', mensaje = "Total de estudiantes matriculados en pregrado por áreas del conocimiento SNIES - sede Palmira", titulo = "Sede estudiantes matriculados pregrado por áreas del conocimiento SNIES - sede Palmira");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por áreas del conocimiento SNIES - sede Palmira", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por áreas del conocimiento SNIES - sede Palmira", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Pal/Pregrado", "Serie.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Pal/Pregrado", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Pal/Pregrado", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Pal/Pregrado", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Pal/Pregrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Pal/Pregrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Pal/Pregrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Pal/Pregrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Pal/Pregrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Pal/Pregrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Pal/Pregrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Pal/Pregrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Pal/Pregrado", "A_edad.html")
Salvar(ESTRATO_TABLA, "G_Matriculados/Pal/Pregrado", "T_estrato.html")
Salvar(ESTRATO_SERIE, "G_Matriculados/Pal/Pregrado", "S_estrato.html")
Salvar(ESTRATO_ACTUAL, "G_Matriculados/Pal/Pregrado", "A_estrato.html")
Salvar(TIPO_COL_TABLA, "G_Matriculados/Pal/Pregrado", "T_colegio.html")
Salvar(TIPO_COL_SERIE, "G_Matriculados/Pal/Pregrado", "S_colegio.html")
Salvar(TIPO_COL_ACTUAL, "G_Matriculados/Pal/Pregrado", "A_colegio.html")
Salvar(PBM_TABLA, "G_Matriculados/Pal/Pregrado", "T_pbm.html")
Salvar(PBM_SERIE, "G_Matriculados/Pal/Pregrado", "S_pbm.html")
Salvar(PBM_ACTUAL, "G_Matriculados/Pal/Pregrado", "A_pbm.html")
Salvar(MAT_PVEZ_TABLA, "G_Matriculados/Pal/Pregrado", "T_mpv.html")
Salvar(MAT_PVEZ_SERIE, "G_Matriculados/Pal/Pregrado", "S_mpv.html")
Salvar(MAT_PVEZ_ACTUAL, "G_Matriculados/Pal/Pregrado", "A_mpv.html")
Salvar(MOD_ADM_TABLA, "G_Matriculados/Pal/Pregrado", "T_modalidad.html")
Salvar(MOD_ADM_SERIE, "G_Matriculados/Pal/Pregrado", "S_modalidad.html")
Salvar(MOD_ADM_ACTUAL, "G_Matriculados/Pal/Pregrado", "A_modalidad.html")
Salvar(TIPO_ADM_TABLA, "G_Matriculados/Pal/Pregrado", "T_tipo.html")
Salvar(TIPO_ADM_SERIE, "G_Matriculados/Pal/Pregrado", "S_tipo.html")
Salvar(TIPO_ADM_ACTUAL, "G_Matriculados/Pal/Pregrado", "A_tipo.html")
Salvar(PAES_TABLA, "G_Matriculados/Pal/Pregrado", "T_paes.html")
Salvar(PAES_SERIE, "G_Matriculados/Pal/Pregrado", "S_paes.html")
Salvar(PAES_ACTUAL, "G_Matriculados/Pal/Pregrado", "A_paes.html")
Salvar(PEAMA_TABLA, "G_Matriculados/Pal/Pregrado", "T_peama.html")
Salvar(PEAMA_SERIE, "G_Matriculados/Pal/Pregrado", "S_peama.html")
Salvar(PEAMA_ACTUAL, "G_Matriculados/Pal/Pregrado", "A_peama.html")
Salvar(MOV_PEAMA_TABLA, "G_Matriculados/Pal/Pregrado", "T_etapa.html")
Salvar(MOV_PEAMA_SERIE, "G_Matriculados/Pal/Pregrado", "S_etapa.html")
Salvar(MOV_PEAMA_ACTUAL, "G_Matriculados/Pal/Pregrado", "A_etapa.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Pal/Pregrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Pal/Pregrado", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Pal/Pregrado", "A_snies.html")


# MATRICULA POSGRADO ---- 


# Mt_Pos1100 ----


# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPos1100") %>% select(-(Nivel))


# Evolución histórica  ---

col <-   c("#f15a24") # Naranja, Total

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados en postgrado ", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#fbb03b", # amarillo, Especialidades médicas
            "#29abe2", # azul claro, Especialización
            "#c1272d")  # rojo, Maestría


NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados en postgrado por nivel de formación', mensaje = "Número de estudiantes matriculados en postgrado por nivel de formación", titulo = "Matriculados en postgrado por nivel de formación");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por nivel de formación", eje = "Número de matriculados");NIVEL_SERIE
NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "NIVEL", colores = col, titulo = "Distribución de matriculados en postgrado por nivel de formación", etiqueta = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NIVEL_ACTUAL


# Sede ---


col <-   c( "#29abe2", # azul claro, Amazonía
            "#8cc63f", # verde, Bogotá
            "#c1272d", # rojo, Caribe 
            # "#9e9ac8",  # Morado claro, De la Paz
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellín
            # "#fbb03b", # amarillo, Orinoquía 
            "#93278f" # Morado, Palmira
            # "#6d6666"  # gris, Tumaco 
) 


SEDE_NOMBRE_MAT_TABLA <- tabla(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", variable = 'Total estudiantes matriculados en postgrado por sede', mensaje = "Total de estudiantes matriculados en postgrado por sede", titulo = "Sede estudiantes matriculados postgrado");SEDE_NOMBRE_MAT_TABLA
SEDE_NOMBRE_MAT_SERIE <- series(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por sede", eje = "Número de estudiantes (k: miles)");SEDE_NOMBRE_MAT_SERIE
SEDE_NOMBRE_MAT_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en postgrado por sede", eje = "Número de estudiantes"); SEDE_NOMBRE_MAT_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados en postgrado según nacionalidad', mensaje = "Número de estudiantes matriculados en postgrado por nacionalidad", titulo = "Matriculados según nacionalidad");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado según nacionalidad", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado,  variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados en postgrado según la nacionalidad", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados en postgrado por sexo', mensaje = "Número de estudiantes de postgrado por sexo", titulo = "Estudiantes de postgrado por sexo");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por sexo", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados en postgrado por sexo", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#6d6666", # gris, 
            "#8cc63f", # verde, 
            "#0071bc", # azul vivo, 
            "#f15a24", # naranja,
            "#fbb03b" ) # amarillo, sin información


################ 1. Tabla

CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados en postgrado por grupos de edad', mensaje = "Número de matriculados en postgrado por grupos de edad", titulo = "Matriculados en postgrado por grupos de edad");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por grupos de edad", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en postgrado por grupos de edad", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL


# Matriculados primera vez ---


col <-   c( "#0071bc", # azul vivo, No
            "#8cc63f") # verde, Sí

################ 1. Tabla

MAT_PVEZ_TABLA <- tabla(datos = Consolidado, categoria = "MAT_PVEZ", variable = 'Total estudiantes de postgrado matriculados por primera vez', mensaje = "Número de estudiantes de postgrado matriculados por primera vez", titulo = "Estudiantes de postgrado matriculados por primera vez");MAT_PVEZ_TABLA
MAT_PVEZ_SERIE <- series(datos = Consolidado, categoria = "MAT_PVEZ", colores = col, titulo = "Evolución del número de estudiantes de postgrado matriculados por primera vez", eje = "Número de estudiantes (k: miles)");MAT_PVEZ_SERIE
MAT_PVEZ_ACTUAL <- torta(datos = Consolidado, variable = "MAT_PVEZ", colores = col, titulo = "Distribución de estudiantes matriculados en postgrado por primera vez", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MAT_PVEZ_ACTUAL


# Convenio ---


col <-   c( "#f15a24", # naranja, No
            "#8cc63f", # verde, Sí
            "#0071bc") # Azul vivo, Sin información


CONVENIO_TABLA <- tabla(datos = Consolidado, categoria = "CONVENIO", variable = 'Total estudiantes matriculados en postgrado en convenios', mensaje = "Total de estudiantes matriculados en postgrado en convenios", titulo = "Sede estudiantes matriculados postgrado");CONVENIO_TABLA
CONVENIO_SERIE <- series(datos = Consolidado, categoria = "CONVENIO", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado en convenios", eje = "Número de estudiantes (k: miles)");CONVENIO_SERIE
CONVENIO_ACTUAL <- drilldown_si_no_torta(datos = Consolidado, categoria = "CONVENIO", categoria_drilldown = "TIP_CONVENIO", colores = col, titulo = "Distribución de matriculados en postgrado en convenios, periodo", titulo_drilldown = "Matriculados", etiqueta = "Total de matriculados", eje = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);CONVENIO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales   


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados en postgrado por áreas del conocimiento SNIES', mensaje = "Total de estudiantes matriculados en postgrado por áreas del conocimiento SNIES", titulo = "Sede estudiantes matriculados postgrado por áreas del conocimiento SNIES");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por áreas del conocimiento SNIES", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_BARRA <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en postgrado por áreas del conocimiento SNIES", eje = "Número de estudiantes"); AREAC_SNIES_BARRA


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Nal/Postgrado", "Serie.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Nal/Postgrado", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Nal/Postgrado", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Nal/Postgrado", "A_nivel.html")
Salvar(SEDE_NOMBRE_MAT_TABLA, "G_Matriculados/Nal/Postgrado", "T_sede.html")
Salvar(SEDE_NOMBRE_MAT_SERIE, "G_Matriculados/Nal/Postgrado", "S_sede.html")
Salvar(SEDE_NOMBRE_MAT_ACTUAL, "G_Matriculados/Nal/Postgrado", "A_sede.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Nal/Postgrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Nal/Postgrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Nal/Postgrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Nal/Postgrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Nal/Postgrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Nal/Postgrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Nal/Postgrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Nal/Postgrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Nal/Postgrado", "A_edad.html")
Salvar(MAT_PVEZ_TABLA, "G_Matriculados/Nal/Postgrado", "T_mpv.html")
Salvar(MAT_PVEZ_SERIE, "G_Matriculados/Nal/Postgrado", "S_mpv.html")
Salvar(MAT_PVEZ_ACTUAL, "G_Matriculados/Nal/Postgrado", "A_mpv.html")
Salvar(CONVENIO_TABLA, "G_Matriculados/Nal/Postgrado", "T_convenio.html")
Salvar(CONVENIO_SERIE, "G_Matriculados/Nal/Postgrado", "S_convenio.html")
Salvar(CONVENIO_ACTUAL, "G_Matriculados/Nal/Postgrado", "A_convenio.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Nal/Postgrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Nal/Postgrado", "S_snies.html")
Salvar(AREAC_SNIES_BARRA, "G_Matriculados/Nal/Postgrado", "A_snies.html")


# Mt_Pos1101 ----


# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPos1101") %>% select(-(Nivel))


# Evolución histórica  ---

col <-   c("#8cc63f") # Verde, Total sede Bogotá

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados en postgrado - sede Bogotá ", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#fbb03b", # amarillo, Especialidades médicas
            "#29abe2", # azul claro, Especialización
            "#c1272d")  # rojo, Maestría


NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados en postgrado por nivel de formación - sede Bogotá', mensaje = "Número de estudiantes matriculados en postgrado por nivel de formación - sede Bogotá", titulo = "Matriculados en postgrado por nivel de formación - sede Bogotá");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por nivel de formación - sede Bogotá", eje = "Número de matriculados");NIVEL_SERIE
NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "NIVEL", colores = col, titulo = "Distribución de matriculados en postgrado por nivel de formación - sede Bogotá", etiqueta = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NIVEL_ACTUAL


# Facultad ---

col <-   c( "#9e9ac8",  # Morado claro, Enfermería
            "#0071bc", # azul vivo, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias económicas
            "#f15a24", # naranja, Ciencias humanas
            "#fbb03b", # amarillo, Derecho, ciencias políticas y sociales
            "#93278f", # Morado, Artes 
            "#8cc63f", # verde, Ingeniería
            "#bdbdbd",  # Gris claro, Medicina 
            "#c1272d", # rojo, Medicina veterianaria y zootecnia
            "#99d8c9"  # Agua Marina, Odontología 
            
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados en postgrado por facultad - sede Bogotá', mensaje = "Total de estudiantes matriculados en postgrado por facultad - sede Bogotá", titulo = "Facultad estudiantes matriculados en postgrado - sede Bogotá");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por facultad - sede Bogotá", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en postgrado por facultad - sede Bogotá", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL



# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados en postgrado según nacionalidad - sede Bogotá', mensaje = "Número de estudiantes matriculados en postgrado por nacionalidad - sede Bogotá", titulo = "Matriculados según nacionalidad - sede Bogotá");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado según nacionalidad - sede Bogotá", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado,  variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados en postgrado según la nacionalidad - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados en postgrado por sexo - sede Bogotá', mensaje = "Número de estudiantes de postgrado por sexo - sede Bogotá", titulo = "Estudiantes de postgrado por sexo - sede Bogotá");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por sexo - sede Bogotá", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados en postgrado por sexo - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#6d6666", # gris, 
            "#8cc63f", # verde, 
            "#0071bc", # azul vivo, 
            "#f15a24", # naranja,
            "#fbb03b" ) # amarillo, sin información


################ 1. Tabla

CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados en postgrado por grupos de edad - sede Bogotá', mensaje = "Número de matriculados en postgrado por grupos de edad - sede Bogotá", titulo = "Matriculados en postgrado por grupos de edad - sede Bogotá");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por grupos de edad - sede Bogotá", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en postgrado por grupos de edad - sede Bogotá", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL


# Matriculados primera vez ---


col <-   c( "#0071bc", # azul vivo, No
            "#8cc63f") # verde, Sí

################ 1. Tabla

MAT_PVEZ_TABLA <- tabla(datos = Consolidado, categoria = "MAT_PVEZ", variable = 'Total estudiantes de postgrado matriculados por primera vez - sede Bogotá', mensaje = "Número de estudiantes de postgrado matriculados por primera vez - sede Bogotá", titulo = "Estudiantes de postgrado matriculados por primera vez - sede Bogotá");MAT_PVEZ_TABLA
MAT_PVEZ_SERIE <- series(datos = Consolidado, categoria = "MAT_PVEZ", colores = col, titulo = "Evolución del número de estudiantes de postgrado matriculados por primera vez - sede Bogotá", eje = "Número de estudiantes (k: miles)");MAT_PVEZ_SERIE
MAT_PVEZ_ACTUAL <- torta(datos = Consolidado, variable = "MAT_PVEZ", colores = col, titulo = "Distribución de estudiantes matriculados en postgrado por primera vez - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MAT_PVEZ_ACTUAL


# Convenio ---


col <-   c( "#f15a24", # naranja, No
            "#8cc63f", # verde, Sí
            "#0071bc") # Azul vivo, Sin información


CONVENIO_TABLA <- tabla(datos = Consolidado, categoria = "CONVENIO", variable = 'Total estudiantes matriculados en postgrado en convenios - sede Bogotá', mensaje = "Total de estudiantes matriculados en postgrado en convenios - sede Bogotá", titulo = "Sede estudiantes matriculados postgrado - sede Bogotá");CONVENIO_TABLA
CONVENIO_SERIE <- series(datos = Consolidado, categoria = "CONVENIO", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado en convenios - sede Bogotá", eje = "Número de estudiantes (k: miles)");CONVENIO_SERIE
CONVENIO_ACTUAL <- drilldown_si_no_torta(datos = Consolidado, categoria = "CONVENIO", categoria_drilldown = "TIP_CONVENIO", colores = col, titulo = "Distribución de matriculados en postgrado en convenios - sede Bogotá, periodo", titulo_drilldown = "Matriculados - sede Bogotá", etiqueta = "Total de matriculados - sede Bogotá", eje = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);CONVENIO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales   


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados en postgrado por áreas del conocimiento SNIES - sede Bogotá', mensaje = "Total de estudiantes matriculados en postgrado por áreas del conocimiento SNIES - sede Bogotá", titulo = "Sede estudiantes matriculados postgrado por áreas del conocimiento SNIES - sede Bogotá");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_BARRA <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en postgrado por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de estudiantes"); AREAC_SNIES_BARRA


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Bog/Postgrado", "Serie.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Bog/Postgrado", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Bog/Postgrado", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Bog/Postgrado", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Bog/Postgrado", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Bog/Postgrado", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Bog/Postgrado", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Bog/Postgrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Bog/Postgrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Bog/Postgrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Bog/Postgrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Bog/Postgrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Bog/Postgrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Bog/Postgrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Bog/Postgrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Bog/Postgrado", "A_edad.html")
Salvar(MAT_PVEZ_TABLA, "G_Matriculados/Bog/Postgrado", "T_mpv.html")
Salvar(MAT_PVEZ_SERIE, "G_Matriculados/Bog/Postgrado", "S_mpv.html")
Salvar(MAT_PVEZ_ACTUAL, "G_Matriculados/Bog/Postgrado", "A_mpv.html")
Salvar(CONVENIO_TABLA, "G_Matriculados/Bog/Postgrado", "T_convenio.html")
Salvar(CONVENIO_SERIE, "G_Matriculados/Bog/Postgrado", "S_convenio.html")
Salvar(CONVENIO_ACTUAL, "G_Matriculados/Bog/Postgrado", "A_convenio.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Bog/Postgrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Bog/Postgrado", "S_snies.html")
Salvar(AREAC_SNIES_BARRA, "G_Matriculados/Bog/Postgrado", "A_snies.html")


# Mt_Pos1102 ----


# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPos1102") %>% select(-(Nivel))


# Evolución histórica  ---

col <-   c("#f15a24") # Naranja, Total sede Medellín

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados en postgrado - sede Medellín ", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Nivel de Formación ---


col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d", # rojo, Maestría
            "#fbb03b") # amarillo, Especialidades médicas
           


NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados en postgrado por nivel de formación - sede Medellín', mensaje = "Número de estudiantes matriculados en postgrado por nivel de formación - sede Medellín", titulo = "Matriculados en postgrado por nivel de formación - sede Medellín");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por nivel de formación - sede Medellín", eje = "Número de matriculados");NIVEL_SERIE
NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "NIVEL", colores = col, titulo = "Distribución de matriculados en postgrado por nivel de formación - sede Medellín", etiqueta = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NIVEL_ACTUAL


# Facultad ---

col <-   c( "#9e9ac8",  # Morado claro, Arquitectura
            "#f15a24", # naranja, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias humanas y económicas 
            "#8cc63f" # verde, Minas
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados en postgrado por facultad - sede Medellín', mensaje = "Total de estudiantes matriculados en postgrado por facultad - sede Medellín", titulo = "Facultad estudiantes matriculados en postgrado - sede Medellín");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por facultad - sede Medellín", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en postgrado por facultad - sede Medellín", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL



# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados en postgrado según nacionalidad - sede Medellín', mensaje = "Número de estudiantes matriculados en postgrado por nacionalidad - sede Medellín", titulo = "Matriculados según nacionalidad - sede Medellín");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado según nacionalidad - sede Medellín", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado,  variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados en postgrado según la nacionalidad - sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados en postgrado por sexo - sede Medellín', mensaje = "Número de estudiantes de postgrado por sexo - sede Medellín", titulo = "Estudiantes de postgrado por sexo - sede Medellín");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por sexo - sede Medellín", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados en postgrado por sexo - sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#6d6666", # gris, 
            "#8cc63f", # verde, 
            "#0071bc", # azul vivo, 
            "#f15a24", # naranja,
            "#fbb03b" ) # amarillo, sin información


################ 1. Tabla

CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados en postgrado por grupos de edad - sede Medellín', mensaje = "Número de matriculados en postgrado por grupos de edad - sede Medellín", titulo = "Matriculados en postgrado por grupos de edad - sede Medellín");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por grupos de edad - sede Medellín", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en postgrado por grupos de edad - sede Medellín", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL


# Matriculados primera vez ---


col <-   c( "#0071bc", # azul vivo, No
            "#8cc63f") # verde, Sí

################ 1. Tabla

MAT_PVEZ_TABLA <- tabla(datos = Consolidado, categoria = "MAT_PVEZ", variable = 'Total estudiantes de postgrado matriculados por primera vez - sede Medellín', mensaje = "Número de estudiantes de postgrado matriculados por primera vez - sede Medellín", titulo = "Estudiantes de postgrado matriculados por primera vez - sede Medellín");MAT_PVEZ_TABLA
MAT_PVEZ_SERIE <- series(datos = Consolidado, categoria = "MAT_PVEZ", colores = col, titulo = "Evolución del número de estudiantes de postgrado matriculados por primera vez - sede Medellín", eje = "Número de estudiantes (k: miles)");MAT_PVEZ_SERIE
MAT_PVEZ_ACTUAL <- torta(datos = Consolidado, variable = "MAT_PVEZ", colores = col, titulo = "Distribución de estudiantes matriculados en postgrado por primera vez - sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MAT_PVEZ_ACTUAL


# Convenio ---


col <-   c( "#f15a24", # naranja, No
            "#8cc63f", # verde, Sí
            "#0071bc") # Azul vivo, Sin información


CONVENIO_TABLA <- tabla(datos = Consolidado, categoria = "CONVENIO", variable = 'Total estudiantes matriculados en postgrado en convenios - sede Medellín', mensaje = "Total de estudiantes matriculados en postgrado en convenios - sede Medellín", titulo = "Sede estudiantes matriculados postgrado - sede Medellín");CONVENIO_TABLA
CONVENIO_SERIE <- series(datos = Consolidado, categoria = "CONVENIO", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado en convenios - sede Medellín", eje = "Número de estudiantes (k: miles)");CONVENIO_SERIE
CONVENIO_ACTUAL <- drilldown_si_no_torta(datos = Consolidado, categoria = "CONVENIO", categoria_drilldown = "TIP_CONVENIO", colores = col, titulo = "Distribución de matriculados en postgrado en convenios - sede Medellín, periodo", titulo_drilldown = "Matriculados - sede Medellín", etiqueta = "Total de matriculados - sede Medellín", eje = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);CONVENIO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales   


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados en postgrado por áreas del conocimiento SNIES - sede Medellín', mensaje = "Total de estudiantes matriculados en postgrado por áreas del conocimiento SNIES - sede Medellín", titulo = "Sede estudiantes matriculados postgrado por áreas del conocimiento SNIES - sede Medellín");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por áreas del conocimiento SNIES - sede Medellín", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_BARRA <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en postgrado por áreas del conocimiento SNIES - sede Medellín", eje = "Número de estudiantes"); AREAC_SNIES_BARRA


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Med/Postgrado", "Serie.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Med/Postgrado", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Med/Postgrado", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Med/Postgrado", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Med/Postgrado", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Med/Postgrado", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Med/Postgrado", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Med/Postgrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Med/Postgrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Med/Postgrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Med/Postgrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Med/Postgrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Med/Postgrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Med/Postgrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Med/Postgrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Med/Postgrado", "A_edad.html")
Salvar(MAT_PVEZ_TABLA, "G_Matriculados/Med/Postgrado", "T_mpv.html")
Salvar(MAT_PVEZ_SERIE, "G_Matriculados/Med/Postgrado", "S_mpv.html")
Salvar(MAT_PVEZ_ACTUAL, "G_Matriculados/Med/Postgrado", "A_mpv.html")
Salvar(CONVENIO_TABLA, "G_Matriculados/Med/Postgrado", "T_convenio.html")
Salvar(CONVENIO_SERIE, "G_Matriculados/Med/Postgrado", "S_convenio.html")
Salvar(CONVENIO_ACTUAL, "G_Matriculados/Med/Postgrado", "A_convenio.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Med/Postgrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Med/Postgrado", "S_snies.html")
Salvar(AREAC_SNIES_BARRA, "G_Matriculados/Med/Postgrado", "A_snies.html")


# Mt_Pos1103 ----


# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPos1103") %>% select(-(Nivel))


# Evolución histórica  ---

col <-   c("#0071bc") # Azul vivo, Total sede Manizales

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados en postgrado - sede Manizales ", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d", # rojo, Maestría
            "#fbb03b") # amarillo, Especialidades médicas


NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados en postgrado por nivel de formación - sede Manizales', mensaje = "Número de estudiantes matriculados en postgrado por nivel de formación - sede Manizales", titulo = "Matriculados en postgrado por nivel de formación - sede Manizales");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por nivel de formación - sede Manizales", eje = "Número de matriculados");NIVEL_SERIE
NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "NIVEL", colores = col, titulo = "Distribución de matriculados en postgrado por nivel de formación - sede Manizales", etiqueta = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NIVEL_ACTUAL


# Facultad ---

col <-   c( "#f15a24", # naranja, Administración
            "#0071bc", # azul vivo, Ciencias exactas y naturales
            "#8cc63f" # verde, Ingeniaría y arquitectura
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados en postgrado por facultad - sede Manizales', mensaje = "Total de estudiantes matriculados en postgrado por facultad - sede Manizales", titulo = "Facultad estudiantes matriculados en postgrado - sede Manizales");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por facultad - sede Manizales", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en postgrado por facultad - sede Manizales", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL



# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados en postgrado según nacionalidad - sede Manizales', mensaje = "Número de estudiantes matriculados en postgrado por nacionalidad - sede Manizales", titulo = "Matriculados según nacionalidad - sede Manizales");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado según nacionalidad - sede Manizales", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado,  variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados en postgrado según la nacionalidad - sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados en postgrado por sexo - sede Manizales', mensaje = "Número de estudiantes de postgrado por sexo - sede Manizales", titulo = "Estudiantes de postgrado por sexo - sede Manizales");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por sexo - sede Manizales", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados en postgrado por sexo - sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#6d6666", # gris, 
            "#8cc63f", # verde, 
            "#0071bc", # azul vivo, 
            "#f15a24", # naranja,
            "#fbb03b" ) # amarillo, sin información


################ 1. Tabla

CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados en postgrado por grupos de edad - sede Manizales', mensaje = "Número de matriculados en postgrado por grupos de edad - sede Manizales", titulo = "Matriculados en postgrado por grupos de edad - sede Manizales");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por grupos de edad - sede Manizales", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en postgrado por grupos de edad - sede Manizales", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL


# Matriculados primera vez ---


col <-   c( "#0071bc", # azul vivo, No
            "#8cc63f") # verde, Sí

################ 1. Tabla

MAT_PVEZ_TABLA <- tabla(datos = Consolidado, categoria = "MAT_PVEZ", variable = 'Total estudiantes de postgrado matriculados por primera vez - sede Manizales', mensaje = "Número de estudiantes de postgrado matriculados por primera vez - sede Manizales", titulo = "Estudiantes de postgrado matriculados por primera vez - sede Manizales");MAT_PVEZ_TABLA
MAT_PVEZ_SERIE <- series(datos = Consolidado, categoria = "MAT_PVEZ", colores = col, titulo = "Evolución del número de estudiantes de postgrado matriculados por primera vez - sede Manizales", eje = "Número de estudiantes (k: miles)");MAT_PVEZ_SERIE
MAT_PVEZ_ACTUAL <- torta(datos = Consolidado, variable = "MAT_PVEZ", colores = col, titulo = "Distribución de estudiantes matriculados en postgrado por primera vez - sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MAT_PVEZ_ACTUAL


# Convenio ---


col <-   c( "#f15a24", # naranja, No
            "#8cc63f", # verde, Sí
            "#0071bc") # Azul vivo, Sin información


CONVENIO_TABLA <- tabla(datos = Consolidado, categoria = "CONVENIO", variable = 'Total estudiantes matriculados en postgrado en convenios - sede Manizales', mensaje = "Total de estudiantes matriculados en postgrado en convenios - sede Manizales", titulo = "Sede estudiantes matriculados postgrado - sede Manizales");CONVENIO_TABLA
CONVENIO_SERIE <- series(datos = Consolidado, categoria = "CONVENIO", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado en convenios - sede Manizales", eje = "Número de estudiantes (k: miles)");CONVENIO_SERIE
CONVENIO_ACTUAL <- drilldown_si_no_torta(datos = Consolidado, categoria = "CONVENIO", categoria_drilldown = "TIP_CONVENIO", colores = col, titulo = "Distribución de matriculados en postgrado en convenios - sede Manizales, periodo", titulo_drilldown = "Matriculados - sede Manizales", etiqueta = "Total de matriculados - sede Manizales", eje = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);CONVENIO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales   


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados en postgrado por áreas del conocimiento SNIES - sede Manizales', mensaje = "Total de estudiantes matriculados en postgrado por áreas del conocimiento SNIES - sede Manizales", titulo = "Sede estudiantes matriculados postgrado por áreas del conocimiento SNIES - sede Manizales");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por áreas del conocimiento SNIES - sede Manizales", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_BARRA <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en postgrado por áreas del conocimiento SNIES - sede Manizales", eje = "Número de estudiantes"); AREAC_SNIES_BARRA


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Man/Postgrado", "Serie.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Man/Postgrado", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Man/Postgrado", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Man/Postgrado", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Man/Postgrado", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Man/Postgrado", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Man/Postgrado", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Man/Postgrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Man/Postgrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Man/Postgrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Man/Postgrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Man/Postgrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Man/Postgrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Man/Postgrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Man/Postgrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Man/Postgrado", "A_edad.html")
Salvar(MAT_PVEZ_TABLA, "G_Matriculados/Man/Postgrado", "T_mpv.html")
Salvar(MAT_PVEZ_SERIE, "G_Matriculados/Man/Postgrado", "S_mpv.html")
Salvar(MAT_PVEZ_ACTUAL, "G_Matriculados/Man/Postgrado", "A_mpv.html")
Salvar(CONVENIO_TABLA, "G_Matriculados/Man/Postgrado", "T_convenio.html")
Salvar(CONVENIO_SERIE, "G_Matriculados/Man/Postgrado", "S_convenio.html")
Salvar(CONVENIO_ACTUAL, "G_Matriculados/Man/Postgrado", "A_convenio.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Man/Postgrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Man/Postgrado", "S_snies.html")
Salvar(AREAC_SNIES_BARRA, "G_Matriculados/Man/Postgrado", "A_snies.html")


# Mt_Pos1104 ----


# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPos1104") %>% select(-(Nivel))


# Evolución histórica  ---

col <-   c("#93278f") # Morado, Total sede Palmira


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados en postgrado - sede Palmira ", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d", # rojo, Maestría
            "#fbb03b") # amarillo, Especialidades médicas


NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados en postgrado por nivel de formación - sede Palmira', mensaje = "Número de estudiantes matriculados en postgrado por nivel de formación - sede Palmira", titulo = "Matriculados en postgrado por nivel de formación - sede Palmira");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por nivel de formación - sede Palmira", eje = "Número de matriculados");NIVEL_SERIE
NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "NIVEL", colores = col, titulo = "Distribución de matriculados en postgrado por nivel de formación - sede Palmira", etiqueta = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NIVEL_ACTUAL


# Facultad ---

col <-   c( "#0071bc", # azul vivo, Ciencias agropecuarias 
            "#8cc63f" # verde, Ingeniería y Administración
)


FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados en postgrado por facultad - sede Palmira', mensaje = "Total de estudiantes matriculados en postgrado por facultad - sede Palmira", titulo = "Facultad estudiantes matriculados en postgrado - sede Palmira");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por facultad - sede Palmira", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en postgrado por facultad - sede Palmira", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL



# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados en postgrado según nacionalidad - sede Palmira', mensaje = "Número de estudiantes matriculados en postgrado por nacionalidad - sede Palmira", titulo = "Matriculados según nacionalidad - sede Palmira");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado según nacionalidad - sede Palmira", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado,  variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados en postgrado según la nacionalidad - sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados en postgrado por sexo - sede Palmira', mensaje = "Número de estudiantes de postgrado por sexo - sede Palmira", titulo = "Estudiantes de postgrado por sexo - sede Palmira");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por sexo - sede Palmira", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados en postgrado por sexo - sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#6d6666", # gris, 
            "#8cc63f", # verde, 
            "#0071bc", # azul vivo, 
            "#f15a24", # naranja,
            "#fbb03b" ) # amarillo, sin información


################ 1. Tabla

CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados en postgrado por grupos de edad - sede Palmira', mensaje = "Número de matriculados en postgrado por grupos de edad - sede Palmira", titulo = "Matriculados en postgrado por grupos de edad - sede Palmira");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por grupos de edad - sede Palmira", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en postgrado por grupos de edad - sede Palmira", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL


# Matriculados primera vez ---


col <-   c( "#0071bc", # azul vivo, No
            "#8cc63f") # verde, Sí

################ 1. Tabla

MAT_PVEZ_TABLA <- tabla(datos = Consolidado, categoria = "MAT_PVEZ", variable = 'Total estudiantes de postgrado matriculados por primera vez - sede Palmira', mensaje = "Número de estudiantes de postgrado matriculados por primera vez - sede Palmira", titulo = "Estudiantes de postgrado matriculados por primera vez - sede Palmira");MAT_PVEZ_TABLA
MAT_PVEZ_SERIE <- series(datos = Consolidado, categoria = "MAT_PVEZ", colores = col, titulo = "Evolución del número de estudiantes de postgrado matriculados por primera vez - sede Palmira", eje = "Número de estudiantes (k: miles)");MAT_PVEZ_SERIE
MAT_PVEZ_ACTUAL <- torta(datos = Consolidado, variable = "MAT_PVEZ", colores = col, titulo = "Distribución de estudiantes matriculados en postgrado por primera vez - sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MAT_PVEZ_ACTUAL


# Convenio ---


col <-   c( "#f15a24", # naranja, No
            "#8cc63f", # verde, Sí
            "#0071bc") # Azul vivo, Sin información


CONVENIO_TABLA <- tabla(datos = Consolidado, categoria = "CONVENIO", variable = 'Total estudiantes matriculados en postgrado en convenios - sede Palmira', mensaje = "Total de estudiantes matriculados en postgrado en convenios - sede Palmira", titulo = "Sede estudiantes matriculados postgrado - sede Palmira");CONVENIO_TABLA
CONVENIO_SERIE <- series(datos = Consolidado, categoria = "CONVENIO", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado en convenios - sede Palmira", eje = "Número de estudiantes (k: miles)");CONVENIO_SERIE
CONVENIO_ACTUAL <- drilldown_si_no_torta(datos = Consolidado, categoria = "CONVENIO", categoria_drilldown = "TIP_CONVENIO", colores = col, titulo = "Distribución de matriculados en postgrado en convenios - sede Palmira, periodo", titulo_drilldown = "Matriculados - sede Palmira", etiqueta = "Total de matriculados - sede Palmira", eje = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);CONVENIO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales   


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados en postgrado por áreas del conocimiento SNIES - sede Palmira', mensaje = "Total de estudiantes matriculados en postgrado por áreas del conocimiento SNIES - sede Palmira", titulo = "Sede estudiantes matriculados postgrado por áreas del conocimiento SNIES - sede Palmira");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por áreas del conocimiento SNIES - sede Palmira", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_BARRA <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en postgrado por áreas del conocimiento SNIES - sede Palmira", eje = "Número de estudiantes"); AREAC_SNIES_BARRA


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Pal/Postgrado", "Serie.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Pal/Postgrado", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Pal/Postgrado", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Pal/Postgrado", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Pal/Postgrado", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Pal/Postgrado", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Pal/Postgrado", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Pal/Postgrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Pal/Postgrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Pal/Postgrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Pal/Postgrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Pal/Postgrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Pal/Postgrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Pal/Postgrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Pal/Postgrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Pal/Postgrado", "A_edad.html")
Salvar(MAT_PVEZ_TABLA, "G_Matriculados/Pal/Postgrado", "T_mpv.html")
Salvar(MAT_PVEZ_SERIE, "G_Matriculados/Pal/Postgrado", "S_mpv.html")
Salvar(MAT_PVEZ_ACTUAL, "G_Matriculados/Pal/Postgrado", "A_mpv.html")
Salvar(CONVENIO_TABLA, "G_Matriculados/Pal/Postgrado", "T_convenio.html")
Salvar(CONVENIO_SERIE, "G_Matriculados/Pal/Postgrado", "S_convenio.html")
Salvar(CONVENIO_ACTUAL, "G_Matriculados/Pal/Postgrado", "A_convenio.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Pal/Postgrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Pal/Postgrado", "S_snies.html")
Salvar(AREAC_SNIES_BARRA, "G_Matriculados/Pal/Postgrado", "A_snies.html")


# MATRICULA PVEZ ---- 

# Mt_Pvez1100 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPvez1100") %>% select(-(Nivel))


col <-   c("#8cc63f") # Verde, Total

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados por primera vez", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE

# Modalidad de formación ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado


TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes matriculados por primera vez por modalidad de formación', mensaje = "Número de estudiantes matriculados por primera vez por modalidad de formación", titulo = "Matriculados por primera vez por modalidad de formación");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por modalidad de formación", eje = "Número de estudiantes  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col, titulo = "Distribución de matriculados por primera vez por modalidad de formación", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#fbb03b", # amarillo, Especialidades médicas
            "#29abe2", # azul claro, Especialización
            "#c1272d", # rojo, Maestría
            "#8cc63f",  # verde, Pregrado
            "#93278f")  # morado, Tecnología


NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados por primera vez por nivel de formación', mensaje = "Número de estudiantes matriculados por primera vez por nivel de formación", titulo = "Matriculados por primera vez por nivel de formación");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por nivel de formación", eje = "Número de estudiantes  (k: miles)");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez por nivel de formación", eje = "Número de estudiantes (k: miles)"); NIVEL_ACTUAL  


# Sedes ---


col <-   c( "#29abe2", # azul claro, Amazonía
            "#8cc63f", # verde, Bogotá
            "#c1272d", # rojo, Caribe 
            "#9e9ac8",  # Morado claro, De la Paz
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellín
            "#fbb03b", # amarillo, Orinoquía 
            "#93278f", # Morado, Palmira
            "#6d6666"  # gris, Tumaco 
) 


SEDE_NOMBRE_MAT_TABLA <- tabla(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", variable = 'Total estudiantes matriculados por primera vez por sedes', mensaje = "Total de estudiantes matriculados por primera vez por sedes", titulo = "Sede estudiantes matriculados por primera vez");SEDE_NOMBRE_MAT_TABLA
SEDE_NOMBRE_MAT_SERIE <- series(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por sedes", eje = "Número de estudiantes (k: miles)");SEDE_NOMBRE_MAT_SERIE
SEDE_NOMBRE_MAT_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez por sedes", eje = "Número de estudiantes (k: miles)"); SEDE_NOMBRE_MAT_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados por primera vez según nacionalidad', mensaje = "Número de estudiantes matriculados por primera vez por nacionalidad", titulo = "Matriculados por primera vez según nacionalidad");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez según nacionalidad", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados por primera vez según la nacionalidad", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por primera vez por sexo', mensaje = "Número de estudiantes matriculados por primera vez por sexo", titulo = "Estudiantes matriculados por primera vez por sexo");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por sexo", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez por sexo", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Área SNIES ---


col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales  


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por primera vez por áreas del conocimiento SNIES', mensaje = "Total de estudiantes matriculados por primera vez por áreas del conocimiento SNIES", titulo = "Sede estudiantes matriculados por primera vez por áreas del conocimiento SNIES");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por áreas del conocimiento SNIES", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez por áreas del conocimiento SNIES", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Nal/Mpv", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Matriculados/Nal/Mpv", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Matriculados/Nal/Mpv", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Matriculados/Nal/Mpv", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Nal/Mpv", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Nal/Mpv", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Nal/Mpv", "A_nivel.html")
Salvar(SEDE_NOMBRE_MAT_TABLA, "G_Matriculados/Nal/Mpv", "T_sede.html")
Salvar(SEDE_NOMBRE_MAT_SERIE, "G_Matriculados/Nal/Mpv", "S_sede.html")
Salvar(SEDE_NOMBRE_MAT_ACTUAL, "G_Matriculados/Nal/Mpv", "A_sede.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Nal/Mpv", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Nal/Mpv", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Nal/Mpv", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Nal/Mpv", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Nal/Mpv", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Nal/Mpv", "A_sexo.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Nal/Mpv", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Nal/Mpv", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Nal/Mpv", "A_snies.html")


# Mt_Pvez1101 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPvez1101") %>% select(-(Nivel))


col <-   c("#8cc63f") # Verde, Total sede Bogotá

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados por primera vez - sede Bogotá", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Modalidad de formación ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado


TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes matriculados por primera vez por modalidad de formación - sede Bogotá', mensaje = "Número de estudiantes matriculados por primera vez por modalidad de formación - sede Bogotá", titulo = "Matriculados por primera vez por modalidad de formación - sede Bogotá");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por modalidad de formación - sede Bogotá", eje = "Número de estudiantes  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col, titulo = "Distribución de matriculados por primera vez por modalidad de formación - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#fbb03b", # amarillo, Especialidades médicas
            "#29abe2", # azul claro, Especialización
            "#c1272d",  # rojo, Maestría
            "#8cc63f",  # verde, Pregrado
            "#93278f") # morado, Tecnología



NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados por primera vez por nivel de formación - sede Bogotá', mensaje = "Número de estudiantes matriculados por primera vez por nivel de formación - sede Bogotá", titulo = "Matriculados por primera vez por nivel de formación - sede Bogotá");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por nivel de formación - sede Bogotá", eje = "Número de estudiantes  (k: miles)");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez por nivel de formación - sede Bogotá", eje = "Número de estudiantes (k: miles)"); NIVEL_ACTUAL  


# Facultad ---

col <-   c( "#9e9ac8",  # Morado claro, Enfermería
            "#0071bc", # azul vivo, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias económicas
            "#f15a24", # naranja, Ciencias humanas
            "#fbb03b", # amarillo, Derecho, ciencias políticas y sociales
            "#93278f", # Morado, Artes 
            "#8cc63f", # verde, Ingeniería
            "#bdbdbd",  # Gris claro, Medicina 
            "#c1272d", # rojo, Medicina veterianaria y zootecnia
            "#99d8c9"  # Agua Marina, Odontología 
            
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados por primera vez por facultad - sede Bogotá', mensaje = "Total de estudiantes matriculados por primera vez por facultad - sede Bogotá", titulo = "Facultad estudiantes matriculados por primera vez - sede Bogotá");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por facultad - sede Bogotá", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez por facultad - sede Bogotá", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados por primera vez según nacionalidad - sede Bogotá', mensaje = "Número de estudiantes matriculados por primera vez por nacionalidad - sede Bogotá", titulo = "Matriculados por primera vez según nacionalidad - sede Bogotá");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez según nacionalidad - sede Bogotá", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados por primera vez según la nacionalidad - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por primera vez por sexo - sede Bogotá', mensaje = "Número de estudiantes matriculados por primera vez por sexo - sede Bogotá", titulo = "Estudiantes matriculados por primera vez por sexo - sede Bogotá");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por sexo - sede Bogotá", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez por sexo - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Área SNIES ---


col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales  


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Bogotá', mensaje = "Total de estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Bogotá", titulo = "Sede estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Bogotá");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Bog/Mpv", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Matriculados/Bog/Mpv", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Matriculados/Bog/Mpv", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Matriculados/Bog/Mpv", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Bog/Mpv", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Bog/Mpv", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Bog/Mpv", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Bog/Mpv", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Bog/Mpv", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Bog/Mpv", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Bog/Mpv", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Bog/Mpv", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Bog/Mpv", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Bog/Mpv", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Bog/Mpv", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Bog/Mpv", "A_sexo.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Bog/Mpv", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Bog/Mpv", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Bog/Mpv", "A_snies.html")



# Mt_Pvez1102 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPvez1102") %>% select(-(Nivel))


col <-   c("#f15a24") # Naranja, Total sede Medellín


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados por primera vez - sede Medellín", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Modalidad de formación ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado


TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes matriculados por primera vez por modalidad de formación - sede Medellín', mensaje = "Número de estudiantes matriculados por primera vez por modalidad de formación - sede Medellín", titulo = "Matriculados por primera vez por modalidad de formación - sede Medellín");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por modalidad de formación - sede Medellín", eje = "Número de estudiantes  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col, titulo = "Distribución de matriculados por primera vez por modalidad de formación - sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d", # rojo, Maestría
            "#8cc63f",  # verde, Pregrado
            "#fbb03b", # amarillo, Especialidades médicas
            "#93278f")  # morado, Tecnología

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados por primera vez por nivel de formación - sede Medellín', mensaje = "Número de estudiantes matriculados por primera vez por nivel de formación - sede Medellín", titulo = "Matriculados por primera vez por nivel de formación - sede Medellín");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por nivel de formación - sede Medellín", eje = "Número de estudiantes  (k: miles)");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez por nivel de formación - sede Medellín", eje = "Número de estudiantes (k: miles)"); NIVEL_ACTUAL  


# Facultad ---

col <-   c( "#9e9ac8",  # Morado claro, Arquitectura
            "#f15a24", # naranja, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias humanas y económicas 
            "#8cc63f" # verde, Minas
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados por primera vez por facultad - sede Medellín', mensaje = "Total de estudiantes matriculados por primera vez por facultad - sede Medellín", titulo = "Facultad estudiantes matriculados por primera vez - sede Medellín");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por facultad - sede Medellín", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez por facultad - sede Medellín", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados por primera vez según nacionalidad - sede Medellín', mensaje = "Número de estudiantes matriculados por primera vez por nacionalidad - sede Medellín", titulo = "Matriculados por primera vez según nacionalidad - sede Medellín");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez según nacionalidad - sede Medellín", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados por primera vez según la nacionalidad - sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por primera vez por sexo - sede Medellín', mensaje = "Número de estudiantes matriculados por primera vez por sexo - sede Medellín", titulo = "Estudiantes matriculados por primera vez por sexo - sede Medellín");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por sexo - sede Medellín", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez por sexo - sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Área SNIES ---


col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales  


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Medellín', mensaje = "Total de estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Medellín", titulo = "Sede estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Medellín");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Medellín", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Medellín", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Med/Mpv", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Matriculados/Med/Mpv", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Matriculados/Med/Mpv", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Matriculados/Med/Mpv", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Med/Mpv", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Med/Mpv", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Med/Mpv", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Med/Mpv", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Med/Mpv", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Med/Mpv", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Med/Mpv", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Med/Mpv", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Med/Mpv", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Med/Mpv", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Med/Mpv", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Med/Mpv", "A_sexo.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Med/Mpv", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Med/Mpv", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Med/Mpv", "A_snies.html")


# Mt_Pvez1103 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPvez1103") %>% select(-(Nivel))


col <-   c("#0071bc") # Azul vivo, Total sede Manizales


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados por primera vez - sede Manizales", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Modalidad de formación ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado


TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes matriculados por primera vez por modalidad de formación - sede Manizales', mensaje = "Número de estudiantes matriculados por primera vez por modalidad de formación - sede Manizales", titulo = "Matriculados por primera vez por modalidad de formación - sede Manizales");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por modalidad de formación - sede Manizales", eje = "Número de estudiantes  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col, titulo = "Distribución de matriculados por primera vez por modalidad de formación - sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d", # rojo, Maestría
            "#8cc63f",  # verde, Pregrado
            "#fbb03b", # amarillo, Especialidades médicas
            "#93278f")  # morado, Tecnología

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados por primera vez por nivel de formación - sede Manizales', mensaje = "Número de estudiantes matriculados por primera vez por nivel de formación - sede Manizales", titulo = "Matriculados por primera vez por nivel de formación - sede Manizales");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por nivel de formación - sede Manizales", eje = "Número de estudiantes  (k: miles)");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez por nivel de formación - sede Manizales", eje = "Número de estudiantes (k: miles)"); NIVEL_ACTUAL  


# Facultad ---

col <-   c( "#f15a24", # naranja, Administración
            "#0071bc", # azul vivo, Ciencias exactas y naturales
            "#8cc63f" # verde, Ingeniaría y arquitectura
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados por primera vez por facultad - sede Manizales', mensaje = "Total de estudiantes matriculados por primera vez por facultad - sede Manizales", titulo = "Facultad estudiantes matriculados por primera vez - sede Manizales");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por facultad - sede Manizales", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez por facultad - sede Manizales", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL



# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados por primera vez según nacionalidad - sede Manizales', mensaje = "Número de estudiantes matriculados por primera vez por nacionalidad - sede Manizales", titulo = "Matriculados por primera vez según nacionalidad - sede Manizales");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez según nacionalidad - sede Manizales", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados por primera vez según la nacionalidad - sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por primera vez por sexo - sede Manizales', mensaje = "Número de estudiantes matriculados por primera vez por sexo - sede Manizales", titulo = "Estudiantes matriculados por primera vez por sexo - sede Manizales");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por sexo - sede Manizales", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez por sexo - sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Área SNIES ---


col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales  


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Manizales', mensaje = "Total de estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Manizales", titulo = "Sede estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Manizales");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Manizales", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Manizales", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Man/Mpv", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Matriculados/Man/Mpv", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Matriculados/Man/Mpv", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Matriculados/Man/Mpv", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Man/Mpv", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Man/Mpv", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Man/Mpv", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Man/Mpv", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Man/Mpv", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Man/Mpv", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Man/Mpv", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Man/Mpv", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Man/Mpv", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Man/Mpv", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Man/Mpv", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Man/Mpv", "A_sexo.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Man/Mpv", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Man/Mpv", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Man/Mpv", "A_snies.html")


# Mt_Pvez1104 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPvez1104") %>% select(-(Nivel))


col <-   c("#93278f") # Morado, Total sede Palmira


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados por primera vez - sede Palmira", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Modalidad de formación ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado


TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes matriculados por primera vez por modalidad de formación - sede Palmira', mensaje = "Número de estudiantes matriculados por primera vez por modalidad de formación - sede Palmira", titulo = "Matriculados por primera vez por modalidad de formación - sede Palmira");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por modalidad de formación - sede Palmira", eje = "Número de estudiantes  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col, titulo = "Distribución de matriculados por primera vez por modalidad de formación - sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d", # rojo, Maestría
            "#8cc63f",  # verde, Pregrado
            "#fbb03b", # amarillo, Especialidades médicas
            "#93278f")  # morado, Tecnología


NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados por primera vez por nivel de formación - sede Palmira', mensaje = "Número de estudiantes matriculados por primera vez por nivel de formación - sede Palmira", titulo = "Matriculados por primera vez por nivel de formación - sede Palmira");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por nivel de formación - sede Palmira", eje = "Número de estudiantes  (k: miles)");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez por nivel de formación - sede Palmira", eje = "Número de estudiantes (k: miles)"); NIVEL_ACTUAL  


# Facultad ---

col <-   c( "#0071bc", # azul vivo, Ciencias agropecuarias 
            "#8cc63f" # verde, Ingeniería y Administración
)


FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados por primera vez por facultad - sede Palmira', mensaje = "Total de estudiantes matriculados por primera vez por facultad - sede Palmira", titulo = "Facultad estudiantes matriculados por primera vez - sede Palmira");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por facultad - sede Palmira", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez por facultad - sede Palmira", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados por primera vez según nacionalidad - sede Palmira', mensaje = "Número de estudiantes matriculados por primera vez por nacionalidad - sede Palmira", titulo = "Matriculados por primera vez según nacionalidad - sede Palmira");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez según nacionalidad - sede Palmira", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados por primera vez según la nacionalidad - sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por primera vez por sexo - sede Palmira', mensaje = "Número de estudiantes matriculados por primera vez por sexo - sede Palmira", titulo = "Estudiantes matriculados por primera vez por sexo - sede Palmira");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por sexo - sede Palmira", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez por sexo - sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Área SNIES ---


col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales  


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Palmira', mensaje = "Total de estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Palmira", titulo = "Sede estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Palmira");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Palmira", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez por áreas del conocimiento SNIES - sede Palmira", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Matriculados/Pal/Mpv", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Matriculados/Pal/Mpv", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Matriculados/Pal/Mpv", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Matriculados/Pal/Mpv", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Pal/Mpv", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Pal/Mpv", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Pal/Mpv", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Pal/Mpv", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Pal/Mpv", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Pal/Mpv", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Pal/Mpv", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Pal/Mpv", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Pal/Mpv", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Pal/Mpv", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Pal/Mpv", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Pal/Mpv", "A_sexo.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Pal/Mpv", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Pal/Mpv", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Pal/Mpv", "A_snies.html")


# MATRICULA PVEZ PRE ---- 


# Mt_Pvez_Pre1100 ----


# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPvezPre1100") %>% select(-(Nivel))


# Evolución histórica---


col <-   c("#8cc63f") # verde, Total


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados por primera vez en pregrado ", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Sede ---



col <-   c( "#29abe2", # azul claro, Amazonía
            "#8cc63f", # verde, Bogotá
            "#c1272d", # rojo, Caribe 
            "#9e9ac8",  # Morado claro, De la Paz
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellín
            "#fbb03b", # amarillo, Orinoquía 
            "#93278f", # Morado, Palmira
            "#6d6666"  # gris, Tumaco 
) 


SEDE_NOMBRE_MAT_TABLA <- tabla(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", variable = 'Total estudiantes matriculados por primera vez en pregrado por sede', mensaje = "Total de estudiantes matriculados por primera vez en pregrado por sede", titulo = "Sede estudiantes matriculados por primera vez en pregrado");SEDE_NOMBRE_MAT_TABLA
SEDE_NOMBRE_MAT_SERIE <- series(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por sede", eje = "Número de estudiantes (k: miles)");SEDE_NOMBRE_MAT_SERIE
SEDE_NOMBRE_MAT_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por sede", eje = "Número de estudiantes"); SEDE_NOMBRE_MAT_ACTUAL


# Nacionalidad ---


col <-   c("#8cc63f", # verde, Colombiana
           "#f15a24", # naranja, Extranjero
           "#0071bc") # azul vivo, Sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados por primera vez en pregrado según nacionalidad', mensaje = "Número de estudiantes matriculados por primera vez en pregrado por nacionalidad", titulo = "Matriculados por primera vez según nacionalidad");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por primera vez según nacionalidad", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados por primera vez en pregrado por nacionalidad", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---


col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres


SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por primera vez en pregrado según sexo', mensaje = "Número de estudiantes matriculados por primera vez en pregrado según sexo", titulo = "Estudiantes matriculados por primera vez de pregrado según sexo");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por sexo", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por sexo", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados por primera vez en pregrado según grupos de edad', mensaje = "Número de matriculados por primera vez en pregrado por grupos de edad", titulo = "Matriculados por primera vez en pregrado por grupos de edad");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por grupos de edad", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por grupos de edad", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL


# Estrato ---

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)

ESTRATO_TABLA <- tabla(datos = Consolidado, categoria = "ESTRATO", variable = 'Total estudiantes matriculados por primera vez en pregrado según estrato', mensaje = "Número de matriculados por primera vez en pregrado por estrato", titulo = "Matriculados por primera vez en pregrado por estrato");ESTRATO_TABLA
ESTRATO_SERIE <- series(datos = Consolidado, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por estrato", eje = "Número de estudiantes (k: miles)");ESTRATO_SERIE
ESTRATO_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por estrato", eje = "Número de estudiantes (k: miles)"); ESTRATO_ACTUAL


# Colegio ---

col <-   c( "#8cc63f", # verde, Oficial
            "#0071bc", # azul vivo, Otros 
            "#f15a24", # naranja, Privado 
            "#6d6666" # gris, Sin información
)


TIPO_COL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_COL", variable = 'Total estudiantes matriculados por primera vez en pregrado según naturaleza del colegio', mensaje = "Número de matriculados en pregrado por primera vez según naturaleza del colegio", titulo = "Matriculados por primera vez en pregrado según naturaleza del colegio");TIPO_COL_TABLA
TIPO_COL_SERIE <- series(datos = Consolidado, categoria = "TIPO_COL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por naturaleza del colegio", eje = "Número de estudiantes (k: miles)");TIPO_COL_SERIE
TIPO_COL_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "TIPO_COL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado según naturaleza del colegio", eje = "Número de estudiantes (k: miles)"); TIPO_COL_ACTUAL


# PBM ---

col <-   c( "#8cc63f", # verde, 11 o menos
            "#6d6666", # gris, 12 a 17
            "#f15a24", # naranja, 18 a 50 
            "#0071bc", # azul vivo, 51 a 100
            "#93278f" # Morado, Sin información
)


PBM_TABLA <- tabla(datos = Consolidado, categoria = "PBM", variable = 'Total estudiantes matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM)', mensaje = "Número de matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM)", titulo = "Matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM)");PBM_TABLA
PBM_SERIE <- series(datos = Consolidado, categoria = "PBM", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por Puntaje Básico Matrícula (PBM)", eje = "Número de estudiantes (k: miles)");PBM_SERIE
PBM_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "PBM", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM)", eje = "Número de estudiantes (k: miles)"); PBM_ACTUAL



# Modalidad de admisión ---

col <-   c( "#f15a24", # naranja, 
            "#8cc63f" # verde, Regular
)


MOD_ADM_TABLA <- tabla(datos = Consolidado, categoria = "MOD_ADM", variable = 'Total estudiantes matriculados por primera vez en pregrado según modalidad de admisión', mensaje = "Número de matriculados por primera vez en pregrado según modalidad de admisión", titulo = "Matriculados por primera vez en pregrado según modalidad de admisión");MOD_ADM_TABLA
MOD_ADM_SERIE <- series(datos = Consolidado, categoria = "MOD_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por modalidad de admisión", eje = "Número de estudiantes (k: miles)");MOD_ADM_SERIE
MOD_ADM_ACTUAL <- torta(datos = Consolidado, variable = "MOD_ADM", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado según modalidad de admisión", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MOD_ADM_ACTUAL


# Tipo de admisión ---

col <-   c( "#f15a24", # naranja, PAES
            "#6d6666", # gris, PEAA
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f" # verde, Regular
)


TIPO_ADM_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_ADM", variable = 'Total estudiantes matriculados por primera vez en pregrado por programa de admisión', mensaje = "Número de matriculados por primera vez en pregrado por programa de admisión", titulo = "Matriculados por primera vez en pregrado por programa de admisión");TIPO_ADM_TABLA
TIPO_ADM_SERIE <- series(datos = Consolidado, categoria = "TIPO_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por programa de admisión", eje = "Número de estudiantes (k: miles)");TIPO_ADM_SERIE
TIPO_ADM_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_ADM", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por programa de admisión", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_ADM_ACTUAL


# PAES ---


col <-   c( "#0071bc", # azul vivo, mejores bachilleres municipios pobres
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666",  # gris, victimas del conflicto
            "#f15a24", # naranja, comunidades indigenas
            "#8cc63f" # verde, población afro 
)


PAES_TABLA <- tabla(datos = Consolidado, categoria = "PAES", variable = 'Total estudiantes matriculados por primera vez en pregrado programa PAES', mensaje = "Número de matriculados por primera vez en pregrado del programa PAES", titulo = "Matriculados por primera vez en pregrado del programa PAES");PAES_TABLA
PAES_SERIE <- series(datos = Consolidado, categoria = "PAES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado del programa PAES", eje = "Número de estudiantes (k: miles)");PAES_SERIE
PAES_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "PAES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado del programa PAES", eje = "Número de estudiantes (k: miles)"); PAES_ACTUAL


# PEAMA ---

col <-   c( "#29abe2", # azul claro, Amazonía
            "#c1272d", # Rojo, Caribe
            "#f15a24", # naranja, Medellín - Sinifaná
            "#fbb03b", # amarillo, Orinoquia
            "#8cc63f", # verde, Bogotá - Sumapaz
            "#0071bc", # azul vivo, Manizales
            "#6d6666" # gris, Tumaco
)


PEAMA_TABLA <- tabla(datos = Consolidado, categoria = "PEAMA", variable = 'Total estudiantes matriculados por primera vez en pregrado programa PEAMA', mensaje = "Número de estudiantes matriculados por primera vez en pregrado programa PEAMA", titulo = "Matriculados por primera vez en pregrado programa PEAMA");PEAMA_TABLA
PEAMA_SERIE <- series(datos = Consolidado, categoria = "PEAMA", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado del programa PEAMA", eje = "Número de matriculados");PEAMA_SERIE
PEAMA_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "PEAMA", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado del programa PEAMA", eje = "Número de estudiantes"); PEAMA_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales  


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por primera vez en pregrado según áreas del conocimiento SNIES', mensaje = "Total de estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES", titulo = "Sede estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Matriculados/Nal/Mpvpre", "Serie.html")
Salvar(SEDE_NOMBRE_MAT_TABLA, "G_Matriculados/Nal/Mpvpre", "T_sede.html")
Salvar(SEDE_NOMBRE_MAT_SERIE, "G_Matriculados/Nal/Mpvpre", "S_sede.html")
Salvar(SEDE_NOMBRE_MAT_ACTUAL, "G_Matriculados/Nal/Mpvpre", "A_sede.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Nal/Mpvpre", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Nal/Mpvpre", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Nal/Mpvpre", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Nal/Mpvpre", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Nal/Mpvpre", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Nal/Mpvpre", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Nal/Mpvpre", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Nal/Mpvpre", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Nal/Mpvpre", "A_edad.html")
Salvar(ESTRATO_TABLA, "G_Matriculados/Nal/Mpvpre", "T_estrato.html")
Salvar(ESTRATO_SERIE, "G_Matriculados/Nal/Mpvpre", "S_estrato.html")
Salvar(ESTRATO_ACTUAL, "G_Matriculados/Nal/Mpvpre", "A_estrato.html")
Salvar(TIPO_COL_TABLA, "G_Matriculados/Nal/Mpvpre", "T_colegio.html")
Salvar(TIPO_COL_SERIE, "G_Matriculados/Nal/Mpvpre", "S_colegio.html")
Salvar(TIPO_COL_ACTUAL, "G_Matriculados/Nal/Mpvpre", "A_colegio.html")
Salvar(PBM_TABLA, "G_Matriculados/Nal/Mpvpre", "T_pbm.html")
Salvar(PBM_SERIE, "G_Matriculados/Nal/Mpvpre", "S_pbm.html")
Salvar(PBM_ACTUAL, "G_Matriculados/Nal/Mpvpre", "A_pbm.html")
Salvar(MOD_ADM_TABLA, "G_Matriculados/Nal/Mpvpre", "T_modalidad.html")
Salvar(MOD_ADM_SERIE, "G_Matriculados/Nal/Mpvpre", "S_modalidad.html")
Salvar(MOD_ADM_ACTUAL, "G_Matriculados/Nal/Mpvpre", "A_modalidad.html")
Salvar(TIPO_ADM_TABLA, "G_Matriculados/Nal/Mpvpre", "T_tipo.html")
Salvar(TIPO_ADM_SERIE, "G_Matriculados/Nal/Mpvpre", "S_tipo.html")
Salvar(TIPO_ADM_ACTUAL, "G_Matriculados/Nal/Mpvpre", "A_tipo.html")
Salvar(PAES_TABLA, "G_Matriculados/Nal/Mpvpre", "T_paes.html")
Salvar(PAES_SERIE, "G_Matriculados/Nal/Mpvpre", "S_paes.html")
Salvar(PAES_ACTUAL, "G_Matriculados/Nal/Mpvpre", "A_paes.html")
Salvar(PEAMA_TABLA, "G_Matriculados/Nal/Mpvpre", "T_peama.html")
Salvar(PEAMA_SERIE, "G_Matriculados/Nal/Mpvpre", "S_peama.html")
Salvar(PEAMA_ACTUAL, "G_Matriculados/Nal/Mpvpre", "A_peama.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Nal/Mpvpre", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Nal/Mpvpre", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Nal/Mpvpre", "A_snies.html")


# Mt_Pvez_Pre1101 ----


# Base de datos agregada nacional


Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPvezPre1101") %>% select(-(Nivel))


# Evolución histórica---


col <-   c("#8cc63f") # Verde, Total


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados por primera vez en pregrado, sede Bogotá", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Facultad ---


col <-   c( "#9e9ac8",  # Morado claro, Enfermería
            "#0071bc", # azul vivo, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias económicas
            "#f15a24", # naranja, Ciencias humanas
            "#fbb03b", # amarillo, Derecho, ciencias políticas y sociales
            "#93278f", # Morado, Artes 
            "#8cc63f", # verde, Ingeniería
            "#bdbdbd",  # Gris claro, Medicina 
            "#c1272d", # rojo, Medicina veterianaria y zootecnia
            "#99d8c9"  # Agua Marina, Odontología 
            
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados por primera vez en pregrado por facultad - sede Bogotá', mensaje = "Total de estudiantes matriculados por primera vez en pregrado por facultad - sede Bogotá", titulo = "Facultad estudiantes matriculados por primera vez en pregrado - sede Bogotá");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por facultad - sede Bogotá", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por facultad - sede Bogotá", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL

# Nacionalidad ---


col <-   c("#8cc63f", # verde, Colombiana
           "#f15a24", # naranja, Extranjero
           "#0071bc") # azul vivo, Sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados por primera vez en pregrado según nacionalidad, sede Bogotá', mensaje = "Número de estudiantes matriculados por primera vez en pregrado por nacionalidad, sede Bogotá", titulo = "Matriculados por primera vez según nacionalidad, sede Bogotá");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por primera vez según nacionalidad, sede Bogotá", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados por primera vez en pregrado por nacionalidad, sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---


col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres


SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por primera vez en pregrado según sexo, sede Bogotá', mensaje = "Número de estudiantes matriculados por primera vez en pregrado según sexo, sede Bogotá", titulo = "Estudiantes matriculados por primera vez de pregrado según sexo, sede Bogotá");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por sexo, sede Bogotá", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por sexo, sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados por primera vez en pregrado según grupos de edad, sede Bogotá', mensaje = "Número de matriculados por primera vez en pregrado por grupos de edad, sede Bogotá", titulo = "Matriculados por primera vez en pregrado por grupos de edad, sede Bogotá");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por grupos de edad, sede Bogotá", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por grupos de edad, sede Bogotá", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL


# Estrato ---

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)

ESTRATO_TABLA <- tabla(datos = Consolidado, categoria = "ESTRATO", variable = 'Total estudiantes matriculados por primera vez en pregrado según estrato, sede Bogotá', mensaje = "Número de matriculados por primera vez en pregrado por estrato, sede Bogotá", titulo = "Matriculados por primera vez en pregrado por estrato, sede Bogotá");ESTRATO_TABLA
ESTRATO_SERIE <- series(datos = Consolidado, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por estrato, sede Bogotá", eje = "Número de estudiantes (k: miles)");ESTRATO_SERIE
ESTRATO_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por estrato, sede Bogotá", eje = "Número de estudiantes (k: miles)"); ESTRATO_ACTUAL


# Colegio ---

col <-   c( "#8cc63f", # verde, Oficial
            "#0071bc", # azul vivo, Otros 
            "#f15a24", # naranja, Privado 
            "#6d6666" # gris, Sin información
)


TIPO_COL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_COL", variable = 'Total estudiantes matriculados por primera vez en pregrado según naturaleza del colegio, sede Bogotá', mensaje = "Número de matriculados en pregrado por primera vez según naturaleza del colegio, sede Bogotá", titulo = "Matriculados por primera vez en pregrado según naturaleza del colegio, sede Bogotá");TIPO_COL_TABLA
TIPO_COL_SERIE <- series(datos = Consolidado, categoria = "TIPO_COL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por naturaleza del colegio, sede Bogotá", eje = "Número de estudiantes (k: miles)");TIPO_COL_SERIE
TIPO_COL_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "TIPO_COL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado según naturaleza del colegio, sede Bogotá", eje = "Número de estudiantes (k: miles)"); TIPO_COL_ACTUAL


# PBM ---

col <-   c( "#8cc63f", # verde, 11 o menos
            "#6d6666", # gris, 12 a 17
            "#f15a24", # naranja, 18 a 50 
            "#0071bc", # azul vivo, 51 a 100
            "#93278f" # Morado, Sin información
)


PBM_TABLA <- tabla(datos = Consolidado, categoria = "PBM", variable = 'Total estudiantes matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM), sede Bogotá', mensaje = "Número de matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM), sede Bogotá", titulo = "Matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM), sede Bogotá");PBM_TABLA
PBM_SERIE <- series(datos = Consolidado, categoria = "PBM", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por Puntaje Básico Matrícula (PBM), sede Bogotá", eje = "Número de estudiantes (k: miles)");PBM_SERIE
PBM_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "PBM", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM), sede Bogotá", eje = "Número de estudiantes (k: miles)"); PBM_ACTUAL


# Modalidad de admisión ---

col <-   c( "#f15a24", # naranja, 
            "#8cc63f" # verde, Regular
)


MOD_ADM_TABLA <- tabla(datos = Consolidado, categoria = "MOD_ADM", variable = 'Total estudiantes matriculados por primera vez en pregrado según modalidad de admisión, sede Bogotá', mensaje = "Número de matriculados por primera vez en pregrado según modalidad de admisión, sede Bogotá", titulo = "Matriculados por primera vez en pregrado según modalidad de admisión, sede Bogotá");MOD_ADM_TABLA
MOD_ADM_SERIE <- series(datos = Consolidado, categoria = "MOD_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por modalidad de admisión, sede Bogotá", eje = "Número de estudiantes (k: miles)");MOD_ADM_SERIE
MOD_ADM_ACTUAL <- torta(datos = Consolidado, variable = "MOD_ADM", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado según modalidad de admisión, sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MOD_ADM_ACTUAL


# Tipo de admisión ---

col <-   c( "#f15a24", # naranja, PAES
            "#6d6666", # gris, PEAA
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f" # verde, Regular
)


TIPO_ADM_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_ADM", variable = 'Total estudiantes matriculados por primera vez en pregrado por programa de admisión, sede Bogotá', mensaje = "Número de matriculados por primera vez en pregrado por programa de admisión, sede Bogotá", titulo = "Matriculados por primera vez en pregrado por programa de admisión, sede Bogotá");TIPO_ADM_TABLA
TIPO_ADM_SERIE <- series(datos = Consolidado, categoria = "TIPO_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por programa de admisión, sede Bogotá", eje = "Número de estudiantes (k: miles)");TIPO_ADM_SERIE
TIPO_ADM_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_ADM", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por programa de admisión, sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_ADM_ACTUAL


# PAES ---


col <-   c( "#0071bc", # azul vivo, mejores bachilleres municipios pobres
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666",  # gris, victimas del conflicto
            "#f15a24", # naranja, comunidades indigenas
            "#8cc63f" # verde, población afro 
)


PAES_TABLA <- tabla(datos = Consolidado, categoria = "PAES", variable = 'Total estudiantes matriculados por primera vez en pregrado programa PAES, sede Bogotá', mensaje = "Número de matriculados por primera vez en pregrado del programa PAES, sede Bogotá", titulo = "Matriculados por primera vez en pregrado del programa PAES, sede Bogotá");PAES_TABLA
PAES_SERIE <- series(datos = Consolidado, categoria = "PAES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado del programa PAES, sede Bogotá", eje = "Número de estudiantes (k: miles)");PAES_SERIE
PAES_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "PAES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado del programa PAES, sede Bogotá", eje = "Número de estudiantes (k: miles)"); PAES_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales  


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por primera vez en pregrado según áreas del conocimiento SNIES, sede Bogotá', mensaje = "Total de estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES, sede Bogotá", titulo = "Sede estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES, sede Bogotá");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES, sede Bogotá", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES, sede Bogotá", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Bog/Mpvpre", "Serie.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Bog/Mpvpre", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Bog/Mpvpre", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Bog/Mpvpre", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Bog/Mpvpre", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Bog/Mpvpre", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Bog/Mpvpre", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Bog/Mpvpre", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Bog/Mpvpre", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Bog/Mpvpre", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Bog/Mpvpre", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Bog/Mpvpre", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Bog/Mpvpre", "A_edad.html")
Salvar(ESTRATO_TABLA, "G_Matriculados/Bog/Mpvpre", "T_estrato.html")
Salvar(ESTRATO_SERIE, "G_Matriculados/Bog/Mpvpre", "S_estrato.html")
Salvar(ESTRATO_ACTUAL, "G_Matriculados/Bog/Mpvpre", "A_estrato.html")
Salvar(TIPO_COL_TABLA, "G_Matriculados/Bog/Mpvpre", "T_colegio.html")
Salvar(TIPO_COL_SERIE, "G_Matriculados/Bog/Mpvpre", "S_colegio.html")
Salvar(TIPO_COL_ACTUAL, "G_Matriculados/Bog/Mpvpre", "A_colegio.html")
Salvar(PBM_TABLA, "G_Matriculados/Bog/Mpvpre", "T_pbm.html")
Salvar(PBM_SERIE, "G_Matriculados/Bog/Mpvpre", "S_pbm.html")
Salvar(PBM_ACTUAL, "G_Matriculados/Bog/Mpvpre", "A_pbm.html")
Salvar(MOD_ADM_TABLA, "G_Matriculados/Bog/Mpvpre", "T_modalidad.html")
Salvar(MOD_ADM_SERIE, "G_Matriculados/Bog/Mpvpre", "S_modalidad.html")
Salvar(MOD_ADM_ACTUAL, "G_Matriculados/Bog/Mpvpre", "A_modalidad.html")
Salvar(TIPO_ADM_TABLA, "G_Matriculados/Bog/Mpvpre", "T_tipo.html")
Salvar(TIPO_ADM_SERIE, "G_Matriculados/Bog/Mpvpre", "S_tipo.html")
Salvar(TIPO_ADM_ACTUAL, "G_Matriculados/Bog/Mpvpre", "A_tipo.html")
Salvar(PAES_TABLA, "G_Matriculados/Bog/Mpvpre", "T_paes.html")
Salvar(PAES_SERIE, "G_Matriculados/Bog/Mpvpre", "S_paes.html")
Salvar(PAES_ACTUAL, "G_Matriculados/Bog/Mpvpre", "A_paes.html")
Salvar(PEAMA_TABLA, "G_Matriculados/Bog/Mpvpre", "T_peama.html")
Salvar(PEAMA_SERIE, "G_Matriculados/Bog/Mpvpre", "S_peama.html")
Salvar(PEAMA_ACTUAL, "G_Matriculados/Bog/Mpvpre", "A_peama.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Bog/Mpvpre", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Bog/Mpvpre", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Bog/Mpvpre", "A_snies.html")


# Mt_Pvez_Pre1102 ----


# Base de datos agregada nacional


Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPvezPre1102") %>% select(-(Nivel))


# Evolución histórica---


col <-   c("#f15a24") # Naranja, Total sede Medellín


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados por primera vez en pregrado, sede Medellín", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Facultad ---


col <-   c( "#9e9ac8",  # Morado claro, Arquitectura
            "#f15a24", # naranja, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias humanas y económicas 
            "#8cc63f" # verde, Minas
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados por primera vez en pregrado por facultad - sede Medellín', mensaje = "Total de estudiantes matriculados por primera vez en pregrado por facultad - sede Medellín", titulo = "Facultad estudiantes matriculados por primera vez en pregrado - sede Medellín");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por facultad - sede Medellín", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por facultad - sede Medellín", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL


# Nacionalidad ---


col <-   c("#8cc63f", # verde, Colombiana
           "#f15a24", # naranja, Extranjero
           "#0071bc") # azul vivo, Sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados por primera vez en pregrado según nacionalidad, sede Medellín', mensaje = "Número de estudiantes matriculados por primera vez en pregrado por nacionalidad, sede Medellín", titulo = "Matriculados por primera vez según nacionalidad, sede Medellín");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por primera vez según nacionalidad, sede Medellín", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados por primera vez en pregrado por nacionalidad, sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---


col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres


SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por primera vez en pregrado según sexo, sede Medellín', mensaje = "Número de estudiantes matriculados por primera vez en pregrado según sexo, sede Medellín", titulo = "Estudiantes matriculados por primera vez de pregrado según sexo, sede Medellín");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por sexo, sede Medellín", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por sexo, sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados por primera vez en pregrado según grupos de edad, sede Medellín', mensaje = "Número de matriculados por primera vez en pregrado por grupos de edad, sede Medellín", titulo = "Matriculados por primera vez en pregrado por grupos de edad, sede Medellín");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por grupos de edad, sede Medellín", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por grupos de edad, sede Medellín", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL


# Estrato ---

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)

ESTRATO_TABLA <- tabla(datos = Consolidado, categoria = "ESTRATO", variable = 'Total estudiantes matriculados por primera vez en pregrado según estrato, sede Medellín', mensaje = "Número de matriculados por primera vez en pregrado por estrato, sede Medellín", titulo = "Matriculados por primera vez en pregrado por estrato, sede Medellín");ESTRATO_TABLA
ESTRATO_SERIE <- series(datos = Consolidado, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por estrato, sede Medellín", eje = "Número de estudiantes (k: miles)");ESTRATO_SERIE
ESTRATO_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por estrato, sede Medellín", eje = "Número de estudiantes (k: miles)"); ESTRATO_ACTUAL


# Colegio ---

col <-   c( "#8cc63f", # verde, Oficial
            "#0071bc", # azul vivo, Otros 
            "#f15a24", # naranja, Privado 
            "#6d6666" # gris, Sin información
)


TIPO_COL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_COL", variable = 'Total estudiantes matriculados por primera vez en pregrado según naturaleza del colegio, sede Medellín', mensaje = "Número de matriculados en pregrado por primera vez según naturaleza del colegio, sede Medellín", titulo = "Matriculados por primera vez en pregrado según naturaleza del colegio, sede Medellín");TIPO_COL_TABLA
TIPO_COL_SERIE <- series(datos = Consolidado, categoria = "TIPO_COL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por naturaleza del colegio, sede Medellín", eje = "Número de estudiantes (k: miles)");TIPO_COL_SERIE
TIPO_COL_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "TIPO_COL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado según naturaleza del colegio, sede Medellín", eje = "Número de estudiantes (k: miles)"); TIPO_COL_ACTUAL


# PBM ---

col <-   c( "#8cc63f", # verde, 11 o menos
            "#6d6666", # gris, 12 a 17
            "#f15a24", # naranja, 18 a 50 
            "#0071bc", # azul vivo, 51 a 100
            "#93278f" # Morado, Sin información
)


PBM_TABLA <- tabla(datos = Consolidado, categoria = "PBM", variable = 'Total estudiantes matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM), sede Medellín', mensaje = "Número de matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM), sede Medellín", titulo = "Matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM), sede Medellín");PBM_TABLA
PBM_SERIE <- series(datos = Consolidado, categoria = "PBM", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por Puntaje Básico Matrícula (PBM), sede Medellín", eje = "Número de estudiantes (k: miles)");PBM_SERIE
PBM_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "PBM", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM), sede Medellín", eje = "Número de estudiantes (k: miles)"); PBM_ACTUAL


# Modalidad de admisión ---

col <-   c( "#f15a24", # naranja, 
            "#8cc63f" # verde, Regular
)


MOD_ADM_TABLA <- tabla(datos = Consolidado, categoria = "MOD_ADM", variable = 'Total estudiantes matriculados por primera vez en pregrado según modalidad de admisión, sede Medellín', mensaje = "Número de matriculados por primera vez en pregrado según modalidad de admisión, sede Medellín", titulo = "Matriculados por primera vez en pregrado según modalidad de admisión, sede Medellín");MOD_ADM_TABLA
MOD_ADM_SERIE <- series(datos = Consolidado, categoria = "MOD_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por modalidad de admisión, sede Medellín", eje = "Número de estudiantes (k: miles)");MOD_ADM_SERIE
MOD_ADM_ACTUAL <- torta(datos = Consolidado, variable = "MOD_ADM", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado según modalidad de admisión, sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MOD_ADM_ACTUAL


# Tipo de admisión ---

col <-   c( "#f15a24", # naranja, PAES
            "#6d6666", # gris, PEAA
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f" # verde, Regular
)


TIPO_ADM_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_ADM", variable = 'Total estudiantes matriculados por primera vez en pregrado por programa de admisión, sede Medellín', mensaje = "Número de matriculados por primera vez en pregrado por programa de admisión, sede Medellín", titulo = "Matriculados por primera vez en pregrado por programa de admisión, sede Medellín");TIPO_ADM_TABLA
TIPO_ADM_SERIE <- series(datos = Consolidado, categoria = "TIPO_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por programa de admisión, sede Medellín", eje = "Número de estudiantes (k: miles)");TIPO_ADM_SERIE
TIPO_ADM_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_ADM", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por programa de admisión, sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_ADM_ACTUAL


# PAES ---


col <-   c( "#0071bc", # azul vivo, mejores bachilleres municipios pobres
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666",  # gris, victimas del conflicto
            "#f15a24", # naranja, comunidades indigenas
            "#8cc63f" # verde, población afro 
)


PAES_TABLA <- tabla(datos = Consolidado, categoria = "PAES", variable = 'Total estudiantes matriculados por primera vez en pregrado programa PAES, sede Medellín', mensaje = "Número de matriculados por primera vez en pregrado del programa PAES, sede Medellín", titulo = "Matriculados por primera vez en pregrado del programa PAES, sede Medellín");PAES_TABLA
PAES_SERIE <- series(datos = Consolidado, categoria = "PAES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado del programa PAES, sede Medellín", eje = "Número de estudiantes (k: miles)");PAES_SERIE
PAES_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "PAES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado del programa PAES, sede Medellín", eje = "Número de estudiantes (k: miles)"); PAES_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales  


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por primera vez en pregrado según áreas del conocimiento SNIES, sede Medellín', mensaje = "Total de estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES, sede Medellín", titulo = "Sede estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES, sede Medellín");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES, sede Medellín", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES, sede Medellín", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Med/Mpvpre", "Serie.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Med/Mpvpre", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Med/Mpvpre", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Med/Mpvpre", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Med/Mpvpre", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Med/Mpvpre", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Med/Mpvpre", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Med/Mpvpre", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Med/Mpvpre", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Med/Mpvpre", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Med/Mpvpre", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Med/Mpvpre", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Med/Mpvpre", "A_edad.html")
Salvar(ESTRATO_TABLA, "G_Matriculados/Med/Mpvpre", "T_estrato.html")
Salvar(ESTRATO_SERIE, "G_Matriculados/Med/Mpvpre", "S_estrato.html")
Salvar(ESTRATO_ACTUAL, "G_Matriculados/Med/Mpvpre", "A_estrato.html")
Salvar(TIPO_COL_TABLA, "G_Matriculados/Med/Mpvpre", "T_colegio.html")
Salvar(TIPO_COL_SERIE, "G_Matriculados/Med/Mpvpre", "S_colegio.html")
Salvar(TIPO_COL_ACTUAL, "G_Matriculados/Med/Mpvpre", "A_colegio.html")
Salvar(PBM_TABLA, "G_Matriculados/Med/Mpvpre", "T_pbm.html")
Salvar(PBM_SERIE, "G_Matriculados/Med/Mpvpre", "S_pbm.html")
Salvar(PBM_ACTUAL, "G_Matriculados/Med/Mpvpre", "A_pbm.html")
Salvar(MOD_ADM_TABLA, "G_Matriculados/Med/Mpvpre", "T_modalidad.html")
Salvar(MOD_ADM_SERIE, "G_Matriculados/Med/Mpvpre", "S_modalidad.html")
Salvar(MOD_ADM_ACTUAL, "G_Matriculados/Med/Mpvpre", "A_modalidad.html")
Salvar(TIPO_ADM_TABLA, "G_Matriculados/Med/Mpvpre", "T_tipo.html")
Salvar(TIPO_ADM_SERIE, "G_Matriculados/Med/Mpvpre", "S_tipo.html")
Salvar(TIPO_ADM_ACTUAL, "G_Matriculados/Med/Mpvpre", "A_tipo.html")
Salvar(PAES_TABLA, "G_Matriculados/Med/Mpvpre", "T_paes.html")
Salvar(PAES_SERIE, "G_Matriculados/Med/Mpvpre", "S_paes.html")
Salvar(PAES_ACTUAL, "G_Matriculados/Med/Mpvpre", "A_paes.html")
Salvar(PEAMA_TABLA, "G_Matriculados/Med/Mpvpre", "T_peama.html")
Salvar(PEAMA_SERIE, "G_Matriculados/Med/Mpvpre", "S_peama.html")
Salvar(PEAMA_ACTUAL, "G_Matriculados/Med/Mpvpre", "A_peama.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Med/Mpvpre", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Med/Mpvpre", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Med/Mpvpre", "A_snies.html")



# Mt_Pvez_Pre1103 ----


# Base de datos agregada nacional


Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPvezPre1103") %>% select(-(Nivel))


# Evolución histórica---


col <-   c("#0071bc") # Azul vivo, Total sede Manizales


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados por primera vez en pregrado, sede Manizales", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Facultad ---


col <-   c( "#f15a24", # naranja, Administración
            "#0071bc", # azul vivo, Ciencias exactas y naturales
            "#8cc63f" # verde, Ingeniaría y arquitectura
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados por primera vez en pregrado por facultad - sede Manizales', mensaje = "Total de estudiantes matriculados por primera vez en pregrado por facultad - sede Manizales", titulo = "Facultad estudiantes matriculados por primera vez en pregrado - sede Manizales");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por facultad - sede Manizales", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por facultad - sede Manizales", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL


# Nacionalidad ---


col <-   c("#8cc63f", # verde, Colombiana
           "#f15a24", # naranja, Extranjero
           "#0071bc") # azul vivo, Sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados por primera vez en pregrado según nacionalidad, sede Manizales', mensaje = "Número de estudiantes matriculados por primera vez en pregrado por nacionalidad, sede Manizales", titulo = "Matriculados por primera vez según nacionalidad, sede Manizales");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por primera vez según nacionalidad, sede Manizales", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados por primera vez en pregrado por nacionalidad, sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---


col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres


SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por primera vez en pregrado según sexo, sede Manizales', mensaje = "Número de estudiantes matriculados por primera vez en pregrado según sexo, sede Manizales", titulo = "Estudiantes matriculados por primera vez de pregrado según sexo, sede Manizales");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por sexo, sede Manizales", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por sexo, sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados por primera vez en pregrado según grupos de edad, sede Manizales', mensaje = "Número de matriculados por primera vez en pregrado por grupos de edad, sede Manizales", titulo = "Matriculados por primera vez en pregrado por grupos de edad, sede Manizales");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por grupos de edad, sede Manizales", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por grupos de edad, sede Manizales", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL


# Estrato ---

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)

ESTRATO_TABLA <- tabla(datos = Consolidado, categoria = "ESTRATO", variable = 'Total estudiantes matriculados por primera vez en pregrado según estrato, sede Manizales', mensaje = "Número de matriculados por primera vez en pregrado por estrato, sede Manizales", titulo = "Matriculados por primera vez en pregrado por estrato, sede Manizales");ESTRATO_TABLA
ESTRATO_SERIE <- series(datos = Consolidado, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por estrato, sede Manizales", eje = "Número de estudiantes (k: miles)");ESTRATO_SERIE
ESTRATO_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por estrato, sede Manizales", eje = "Número de estudiantes (k: miles)"); ESTRATO_ACTUAL


# Colegio ---

col <-   c( "#8cc63f", # verde, Oficial
            "#0071bc", # azul vivo, Otros 
            "#f15a24", # naranja, Privado 
            "#6d6666" # gris, Sin información
)


TIPO_COL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_COL", variable = 'Total estudiantes matriculados por primera vez en pregrado según naturaleza del colegio, sede Manizales', mensaje = "Número de matriculados en pregrado por primera vez según naturaleza del colegio, sede Manizales", titulo = "Matriculados por primera vez en pregrado según naturaleza del colegio, sede Manizales");TIPO_COL_TABLA
TIPO_COL_SERIE <- series(datos = Consolidado, categoria = "TIPO_COL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por naturaleza del colegio, sede Manizales", eje = "Número de estudiantes (k: miles)");TIPO_COL_SERIE
TIPO_COL_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "TIPO_COL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado según naturaleza del colegio, sede Manizales", eje = "Número de estudiantes (k: miles)"); TIPO_COL_ACTUAL


# PBM ---

col <-   c( "#8cc63f", # verde, 11 o menos
            "#6d6666", # gris, 12 a 17
            "#f15a24", # naranja, 18 a 50 
            "#0071bc", # azul vivo, 51 a 100
            "#93278f" # Morado, Sin información
)


PBM_TABLA <- tabla(datos = Consolidado, categoria = "PBM", variable = 'Total estudiantes matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM), sede Manizales', mensaje = "Número de matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM), sede Manizales", titulo = "Matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM), sede Manizales");PBM_TABLA
PBM_SERIE <- series(datos = Consolidado, categoria = "PBM", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por Puntaje Básico Matrícula (PBM), sede Manizales", eje = "Número de estudiantes (k: miles)");PBM_SERIE
PBM_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "PBM", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM), sede Manizales", eje = "Número de estudiantes (k: miles)"); PBM_ACTUAL


# Modalidad de admisión ---

col <-   c( "#f15a24", # naranja, 
            "#8cc63f" # verde, Regular
)


MOD_ADM_TABLA <- tabla(datos = Consolidado, categoria = "MOD_ADM", variable = 'Total estudiantes matriculados por primera vez en pregrado según modalidad de admisión, sede Manizales', mensaje = "Número de matriculados por primera vez en pregrado según modalidad de admisión, sede Manizales", titulo = "Matriculados por primera vez en pregrado según modalidad de admisión, sede Manizales");MOD_ADM_TABLA
MOD_ADM_SERIE <- series(datos = Consolidado, categoria = "MOD_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por modalidad de admisión, sede Manizales", eje = "Número de estudiantes (k: miles)");MOD_ADM_SERIE
MOD_ADM_ACTUAL <- torta(datos = Consolidado, variable = "MOD_ADM", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado según modalidad de admisión, sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MOD_ADM_ACTUAL


# Tipo de admisión ---

col <-   c( "#f15a24", # naranja, PAES
            "#6d6666", # gris, PEAA
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f" # verde, Regular
)


TIPO_ADM_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_ADM", variable = 'Total estudiantes matriculados por primera vez en pregrado por programa de admisión, sede Manizales', mensaje = "Número de matriculados por primera vez en pregrado por programa de admisión, sede Manizales", titulo = "Matriculados por primera vez en pregrado por programa de admisión, sede Manizales");TIPO_ADM_TABLA
TIPO_ADM_SERIE <- series(datos = Consolidado, categoria = "TIPO_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por programa de admisión, sede Manizales", eje = "Número de estudiantes (k: miles)");TIPO_ADM_SERIE
TIPO_ADM_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_ADM", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por programa de admisión, sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_ADM_ACTUAL


# PAES ---


col <-   c( "#0071bc", # azul vivo, mejores bachilleres municipios pobres
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666",  # gris, victimas del conflicto
            "#f15a24", # naranja, comunidades indigenas
            "#8cc63f" # verde, población afro 
)


PAES_TABLA <- tabla(datos = Consolidado, categoria = "PAES", variable = 'Total estudiantes matriculados por primera vez en pregrado programa PAES, sede Manizales', mensaje = "Número de matriculados por primera vez en pregrado del programa PAES, sede Manizales", titulo = "Matriculados por primera vez en pregrado del programa PAES, sede Manizales");PAES_TABLA
PAES_SERIE <- series(datos = Consolidado, categoria = "PAES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado del programa PAES, sede Manizales", eje = "Número de estudiantes (k: miles)");PAES_SERIE
PAES_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "PAES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado del programa PAES, sede Manizales", eje = "Número de estudiantes (k: miles)"); PAES_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales  


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por primera vez en pregrado según áreas del conocimiento SNIES, sede Manizales', mensaje = "Total de estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES, sede Manizales", titulo = "Sede estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES, sede Manizales");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES, sede Manizales", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES, sede Manizales", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Man/Mpvpre", "Serie.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Man/Mpvpre", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Man/Mpvpre", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Man/Mpvpre", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Man/Mpvpre", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Man/Mpvpre", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Man/Mpvpre", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Man/Mpvpre", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Man/Mpvpre", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Man/Mpvpre", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Man/Mpvpre", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Man/Mpvpre", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Man/Mpvpre", "A_edad.html")
Salvar(ESTRATO_TABLA, "G_Matriculados/Man/Mpvpre", "T_estrato.html")
Salvar(ESTRATO_SERIE, "G_Matriculados/Man/Mpvpre", "S_estrato.html")
Salvar(ESTRATO_ACTUAL, "G_Matriculados/Man/Mpvpre", "A_estrato.html")
Salvar(TIPO_COL_TABLA, "G_Matriculados/Man/Mpvpre", "T_colegio.html")
Salvar(TIPO_COL_SERIE, "G_Matriculados/Man/Mpvpre", "S_colegio.html")
Salvar(TIPO_COL_ACTUAL, "G_Matriculados/Man/Mpvpre", "A_colegio.html")
Salvar(PBM_TABLA, "G_Matriculados/Man/Mpvpre", "T_pbm.html")
Salvar(PBM_SERIE, "G_Matriculados/Man/Mpvpre", "S_pbm.html")
Salvar(PBM_ACTUAL, "G_Matriculados/Man/Mpvpre", "A_pbm.html")
Salvar(MOD_ADM_TABLA, "G_Matriculados/Man/Mpvpre", "T_modalidad.html")
Salvar(MOD_ADM_SERIE, "G_Matriculados/Man/Mpvpre", "S_modalidad.html")
Salvar(MOD_ADM_ACTUAL, "G_Matriculados/Man/Mpvpre", "A_modalidad.html")
Salvar(TIPO_ADM_TABLA, "G_Matriculados/Man/Mpvpre", "T_tipo.html")
Salvar(TIPO_ADM_SERIE, "G_Matriculados/Man/Mpvpre", "S_tipo.html")
Salvar(TIPO_ADM_ACTUAL, "G_Matriculados/Man/Mpvpre", "A_tipo.html")
Salvar(PAES_TABLA, "G_Matriculados/Man/Mpvpre", "T_paes.html")
Salvar(PAES_SERIE, "G_Matriculados/Man/Mpvpre", "S_paes.html")
Salvar(PAES_ACTUAL, "G_Matriculados/Man/Mpvpre", "A_paes.html")
Salvar(PEAMA_TABLA, "G_Matriculados/Man/Mpvpre", "T_peama.html")
Salvar(PEAMA_SERIE, "G_Matriculados/Man/Mpvpre", "S_peama.html")
Salvar(PEAMA_ACTUAL, "G_Matriculados/Man/Mpvpre", "A_peama.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Man/Mpvpre", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Man/Mpvpre", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Man/Mpvpre", "A_snies.html")


# Mt_Pvez_Pre1104 ----


# Base de datos agregada nacional


Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPvezPre1104") %>% select(-(Nivel))


# Evolución histórica---


col <-   c("#93278f") # Morado, Total sede Palmira


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados por primera vez en pregrado, sede Palmira", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Facultad ---


col <-   c( "#0071bc", # azul vivo, Ciencias agropecuarias 
            "#8cc63f" # verde, Ingeniería y Administración
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados por primera vez en pregrado por facultad - sede Palmira', mensaje = "Total de estudiantes matriculados por primera vez en pregrado por facultad - sede Palmira", titulo = "Facultad estudiantes matriculados por primera vez en pregrado - sede Palmira");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por facultad - sede Palmira", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por facultad - sede Palmira", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL


# Nacionalidad ---


col <-   c("#8cc63f", # verde, Colombiana
           "#f15a24", # naranja, Extranjero
           "#0071bc") # azul vivo, Sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados por primera vez en pregrado según nacionalidad, sede Palmira', mensaje = "Número de estudiantes matriculados por primera vez en pregrado por nacionalidad, sede Palmira", titulo = "Matriculados por primera vez según nacionalidad, sede Palmira");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por primera vez según nacionalidad, sede Palmira", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de matriculados por primera vez en pregrado por nacionalidad, sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---


col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres


SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por primera vez en pregrado según sexo, sede Palmira', mensaje = "Número de estudiantes matriculados por primera vez en pregrado según sexo, sede Palmira", titulo = "Estudiantes matriculados por primera vez de pregrado según sexo, sede Palmira");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por sexo, sede Palmira", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por sexo, sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados por primera vez en pregrado según grupos de edad, sede Palmira', mensaje = "Número de matriculados por primera vez en pregrado por grupos de edad, sede Palmira", titulo = "Matriculados por primera vez en pregrado por grupos de edad, sede Palmira");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por grupos de edad, sede Palmira", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por grupos de edad, sede Palmira", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL


# Estrato ---

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)

ESTRATO_TABLA <- tabla(datos = Consolidado, categoria = "ESTRATO", variable = 'Total estudiantes matriculados por primera vez en pregrado según estrato, sede Palmira', mensaje = "Número de matriculados por primera vez en pregrado por estrato, sede Palmira", titulo = "Matriculados por primera vez en pregrado por estrato, sede Palmira");ESTRATO_TABLA
ESTRATO_SERIE <- series(datos = Consolidado, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por estrato, sede Palmira", eje = "Número de estudiantes (k: miles)");ESTRATO_SERIE
ESTRATO_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por estrato, sede Palmira", eje = "Número de estudiantes (k: miles)"); ESTRATO_ACTUAL


# Colegio ---

col <-   c( "#8cc63f", # verde, Oficial
            "#0071bc", # azul vivo, Otros 
            "#f15a24", # naranja, Privado 
            "#6d6666" # gris, Sin información
)


TIPO_COL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_COL", variable = 'Total estudiantes matriculados por primera vez en pregrado según naturaleza del colegio, sede Palmira', mensaje = "Número de matriculados en pregrado por primera vez según naturaleza del colegio, sede Palmira", titulo = "Matriculados por primera vez en pregrado según naturaleza del colegio, sede Palmira");TIPO_COL_TABLA
TIPO_COL_SERIE <- series(datos = Consolidado, categoria = "TIPO_COL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por naturaleza del colegio, sede Palmira", eje = "Número de estudiantes (k: miles)");TIPO_COL_SERIE
TIPO_COL_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "TIPO_COL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado según naturaleza del colegio, sede Palmira", eje = "Número de estudiantes (k: miles)"); TIPO_COL_ACTUAL


# PBM ---

col <-   c( "#8cc63f", # verde, 11 o menos
            "#6d6666", # gris, 12 a 17
            "#f15a24", # naranja, 18 a 50 
            "#0071bc", # azul vivo, 51 a 100
            "#93278f" # Morado, Sin información
)


PBM_TABLA <- tabla(datos = Consolidado, categoria = "PBM", variable = 'Total estudiantes matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM), sede Palmira', mensaje = "Número de matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM), sede Palmira", titulo = "Matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM), sede Palmira");PBM_TABLA
PBM_SERIE <- series(datos = Consolidado, categoria = "PBM", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por Puntaje Básico Matrícula (PBM), sede Palmira", eje = "Número de estudiantes (k: miles)");PBM_SERIE
PBM_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "PBM", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado según Puntaje Básico Matrícula (PBM), sede Palmira", eje = "Número de estudiantes (k: miles)"); PBM_ACTUAL


# Modalidad de admisión ---

col <-   c( "#f15a24", # naranja, 
            "#8cc63f" # verde, Regular
)


MOD_ADM_TABLA <- tabla(datos = Consolidado, categoria = "MOD_ADM", variable = 'Total estudiantes matriculados por primera vez en pregrado según modalidad de admisión, sede Palmira', mensaje = "Número de matriculados por primera vez en pregrado según modalidad de admisión, sede Palmira", titulo = "Matriculados por primera vez en pregrado según modalidad de admisión, sede Palmira");MOD_ADM_TABLA
MOD_ADM_SERIE <- series(datos = Consolidado, categoria = "MOD_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por modalidad de admisión, sede Palmira", eje = "Número de estudiantes (k: miles)");MOD_ADM_SERIE
MOD_ADM_ACTUAL <- torta(datos = Consolidado, variable = "MOD_ADM", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado según modalidad de admisión, sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MOD_ADM_ACTUAL


# Tipo de admisión ---

col <-   c( "#f15a24", # naranja, PAES
            "#6d6666", # gris, PEAA
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f" # verde, Regular
)


TIPO_ADM_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_ADM", variable = 'Total estudiantes matriculados por primera vez en pregrado por programa de admisión, sede Palmira', mensaje = "Número de matriculados por primera vez en pregrado por programa de admisión, sede Palmira", titulo = "Matriculados por primera vez en pregrado por programa de admisión, sede Palmira");TIPO_ADM_TABLA
TIPO_ADM_SERIE <- series(datos = Consolidado, categoria = "TIPO_ADM", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por programa de admisión, sede Palmira", eje = "Número de estudiantes (k: miles)");TIPO_ADM_SERIE
TIPO_ADM_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_ADM", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por programa de admisión, sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_ADM_ACTUAL


# PAES ---


col <-   c( "#0071bc", # azul vivo, mejores bachilleres municipios pobres
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666",  # gris, victimas del conflicto
            "#f15a24", # naranja, comunidades indigenas
            "#8cc63f" # verde, población afro 
)


PAES_TABLA <- tabla(datos = Consolidado, categoria = "PAES", variable = 'Total estudiantes matriculados por primera vez en pregrado programa PAES, sede Palmira', mensaje = "Número de matriculados por primera vez en pregrado del programa PAES, sede Palmira", titulo = "Matriculados por primera vez en pregrado del programa PAES, sede Palmira");PAES_TABLA
PAES_SERIE <- series(datos = Consolidado, categoria = "PAES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado del programa PAES, sede Palmira", eje = "Número de estudiantes (k: miles)");PAES_SERIE
PAES_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "PAES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado del programa PAES, sede Palmira", eje = "Número de estudiantes (k: miles)"); PAES_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales  


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por primera vez en pregrado según áreas del conocimiento SNIES, sede Palmira', mensaje = "Total de estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES, sede Palmira", titulo = "Sede estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES, sede Palmira");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES, sede Palmira", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en pregrado por áreas del conocimiento SNIES, sede Palmira", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Matriculados/Pal/Mpvpre", "Serie.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Pal/Mpvpre", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Pal/Mpvpre", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Pal/Mpvpre", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Pal/Mpvpre", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Pal/Mpvpre", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Pal/Mpvpre", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Pal/Mpvpre", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Pal/Mpvpre", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Pal/Mpvpre", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Pal/Mpvpre", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Pal/Mpvpre", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Pal/Mpvpre", "A_edad.html")
Salvar(ESTRATO_TABLA, "G_Matriculados/Pal/Mpvpre", "T_estrato.html")
Salvar(ESTRATO_SERIE, "G_Matriculados/Pal/Mpvpre", "S_estrato.html")
Salvar(ESTRATO_ACTUAL, "G_Matriculados/Pal/Mpvpre", "A_estrato.html")
Salvar(TIPO_COL_TABLA, "G_Matriculados/Pal/Mpvpre", "T_colegio.html")
Salvar(TIPO_COL_SERIE, "G_Matriculados/Pal/Mpvpre", "S_colegio.html")
Salvar(TIPO_COL_ACTUAL, "G_Matriculados/Pal/Mpvpre", "A_colegio.html")
Salvar(PBM_TABLA, "G_Matriculados/Pal/Mpvpre", "T_pbm.html")
Salvar(PBM_SERIE, "G_Matriculados/Pal/Mpvpre", "S_pbm.html")
Salvar(PBM_ACTUAL, "G_Matriculados/Pal/Mpvpre", "A_pbm.html")
Salvar(MOD_ADM_TABLA, "G_Matriculados/Pal/Mpvpre", "T_modalidad.html")
Salvar(MOD_ADM_SERIE, "G_Matriculados/Pal/Mpvpre", "S_modalidad.html")
Salvar(MOD_ADM_ACTUAL, "G_Matriculados/Pal/Mpvpre", "A_modalidad.html")
Salvar(TIPO_ADM_TABLA, "G_Matriculados/Pal/Mpvpre", "T_tipo.html")
Salvar(TIPO_ADM_SERIE, "G_Matriculados/Pal/Mpvpre", "S_tipo.html")
Salvar(TIPO_ADM_ACTUAL, "G_Matriculados/Pal/Mpvpre", "A_tipo.html")
Salvar(PAES_TABLA, "G_Matriculados/Pal/Mpvpre", "T_paes.html")
Salvar(PAES_SERIE, "G_Matriculados/Pal/Mpvpre", "S_paes.html")
Salvar(PAES_ACTUAL, "G_Matriculados/Pal/Mpvpre", "A_paes.html")
Salvar(PEAMA_TABLA, "G_Matriculados/Pal/Mpvpre", "T_peama.html")
Salvar(PEAMA_SERIE, "G_Matriculados/Pal/Mpvpre", "S_peama.html")
Salvar(PEAMA_ACTUAL, "G_Matriculados/Pal/Mpvpre", "A_peama.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Pal/Mpvpre", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Pal/Mpvpre", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Pal/Mpvpre", "A_snies.html")


# MATRICULA PVEZ POS ---- 


# Mt_Pvez_Pos1100 ----


# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPvezPos1100") %>% select(-(Nivel))


# Evolución histórica---

col <-   c("#f15a24") # Naranja, Total

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados por primera vez en postgrado ", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#fbb03b", # amarillo, Especialidades médicas
            "#29abe2", # azul claro, Especialización
            "#c1272d")  # rojo, Maestría

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados por primera vez en postgrado por nivel de formación', mensaje = "Número de estudiantes matriculados por primera vez en postgrado por nivel de formación", titulo = "Matriculados por primera vez en postgrado por nivel de formación");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por nivel de formación", eje = "Número de matriculados");NIVEL_SERIE
NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "NIVEL", colores = col, titulo = "Distribución de matriculados por primera vez en postgrado por nivel de formación", etiqueta = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NIVEL_ACTUAL



# Sede ---

col <-   c( "#29abe2", # azul claro, Amazonía
            "#8cc63f", # verde, Bogotá
            "#c1272d", # rojo, Caribe 
            #"#9e9ac8",  # Morado claro, De la Paz
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellín
            #"#fbb03b", # amarillo, Orinoquía 
            "#93278f" # Morado, Palmira
            #"#6d6666"  # gris, Tumaco 
) 


SEDE_NOMBRE_MAT_TABLA <- tabla(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", variable = 'Total estudiantes matriculados por primera vez en postgrado por sede', mensaje = "Total de estudiantes matriculados por primera vez en postgrado por sede", titulo = "Sede estudiantes matriculados por primera vez en postgrado");SEDE_NOMBRE_MAT_TABLA
SEDE_NOMBRE_MAT_SERIE <- series(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por sede", eje = "Número de estudiantes (k: miles)");SEDE_NOMBRE_MAT_SERIE
SEDE_NOMBRE_MAT_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por sede", eje = "Número de estudiantes"); SEDE_NOMBRE_MAT_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información

NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados por primera vez en postgrado según nacionalidad', mensaje = "Número de estudiantes matriculados por primera vez en postgrado por nacionalidad", titulo = "Matriculados por primera vez en postgrado según nacionalidad");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado según nacionalidad", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado según la nacionalidad", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por primera vez en postgrado por sexo', mensaje = "Número de estudiantes matriculados por primera vez en postgrado por sexo", titulo = "Estudiantes de postgrado matriculados por primera vez por sexo");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por sexo", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por sexo", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---


col <-   c( "#6d6666", # gris, 25 años o menos
            "#8cc63f", # verde, 26 a 30 años
            "#0071bc", # azul vivo, 31 a 35 años
            "#f15a24", # naranja, 36 años o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados por primera vez en postgrado por grupos de edad', mensaje = "Número de matriculados por primera vez en postgrado por grupos de edad", titulo = "Matriculados en postgrado por primera vez por grupos de edad");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por grupos de edad", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por grupos de edad", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL



# Convenio ---

col <-   c( "#f15a24", # naranja, No
            "#8cc63f", # verde, Sí
            "#0071bc") # Azul vivo, Sin información


CONVENIO_TABLA <- tabla(datos = Consolidado, categoria = "CONVENIO", variable = 'Total estudiantes matriculados por primera vez en postgrado en convenios', mensaje = "Total de estudiantes matriculados por primera vez en postgrado en convenios", titulo = "Total estudiantes matriculados por primera vez en postgrado en convenios");CONVENIO_TABLA
CONVENIO_SERIE <- series(datos = Consolidado, categoria = "CONVENIO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado en convenios", eje = "Número de estudiantes (k: miles)");CONVENIO_SERIE
CONVENIO_ACTUAL <- drilldown_si_no_torta(datos = Consolidado, categoria = "CONVENIO", categoria_drilldown = "TIP_CONVENIO", colores = col, titulo = "Distribución de matriculados por primera vez en postgrado en convenios, periodo", titulo_drilldown = "Matriculados", etiqueta = "Total de matriculados", eje = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);CONVENIO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales  


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES', mensaje = "Total de estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES", titulo = "Sede estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Nal/Mpvpos", "Serie.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Nal/Mpvpos", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Nal/Mpvpos", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Nal/Mpvpos", "A_nivel.html")
Salvar(SEDE_NOMBRE_MAT_TABLA, "G_Matriculados/Nal/Mpvpos", "T_sede.html")
Salvar(SEDE_NOMBRE_MAT_SERIE, "G_Matriculados/Nal/Mpvpos", "S_sede.html")
Salvar(SEDE_NOMBRE_MAT_ACTUAL, "G_Matriculados/Nal/Mpvpos", "A_sede.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Nal/Mpvpos", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Nal/Mpvpos", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Nal/Mpvpos", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Nal/Mpvpos", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Nal/Mpvpos", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Nal/Mpvpos", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Nal/Mpvpos", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Nal/Mpvpos", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Nal/Mpvpos", "A_edad.html")
Salvar(CONVENIO_TABLA, "G_Matriculados/Nal/Mpvpos", "T_convenio.html")
Salvar(CONVENIO_SERIE, "G_Matriculados/Nal/Mpvpos", "S_convenio.html")
Salvar(CONVENIO_ACTUAL, "G_Matriculados/Nal/Mpvpos", "A_convenio.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Nal/Mpvpos", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Nal/Mpvpos", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Nal/Mpvpos", "A_snies.html")











# Mt_Pvez_Pos1101 ----


# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPvezPos1101") %>% select(-(Nivel))


# Evolución histórica---


col <-   c("#8cc63f") # Verde, Total


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados por primera vez en postgrado - sede Bogotá", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#fbb03b", # amarillo, Especialidades médicas
            "#29abe2", # azul claro, Especialización
            "#c1272d")  # rojo, Maestría

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados por primera vez en postgrado por nivel de formación - sede Bogotá', mensaje = "Número de estudiantes matriculados por primera vez en postgrado por nivel de formación - sede Bogotá", titulo = "Matriculados por primera vez en postgrado por nivel de formación, sede Bogotá");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por nivel de formación - sede Bogotá", eje = "Número de matriculados");NIVEL_SERIE
NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "NIVEL", colores = col, titulo = "Distribución de matriculados por primera vez en postgrado por nivel de formación - sede Bogotá", etiqueta = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NIVEL_ACTUAL


# Facultad ---

col <-   c( "#9e9ac8",  # Morado claro, Enfermería
            "#0071bc", # azul vivo, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias económicas
            "#f15a24", # naranja, Ciencias humanas
            "#fbb03b", # amarillo, Derecho, ciencias políticas y sociales
            "#93278f", # Morado, Artes 
            "#8cc63f", # verde, Ingeniería
            "#bdbdbd",  # Gris claro, Medicina 
            "#c1272d", # rojo, Medicina veterianaria y zootecnia
            "#99d8c9"  # Agua Marina, Odontología 
            
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados por primera vez en postgrado por facultad - sede Bogotá', mensaje = "Total de estudiantes matriculados por primera vez en postgrado por facultad - sede Bogotá", titulo = "Facultad estudiantes matriculados por primera vez en postgrado - sede Bogotá");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por facultad - sede Bogotá", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por facultad - sede Bogotá", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL



# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información

NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados por primera vez en postgrado según nacionalidad - sede Bogotá', mensaje = "Número de estudiantes matriculados por primera vez en postgrado por nacionalidad - sede Bogotá", titulo = "Matriculados por primera vez en postgrado según nacionalidad - sede Bogotá");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado según nacionalidad - sede Bogotá", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado según la nacionalidad - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por primera vez en postgrado por sexo - sede Bogotá', mensaje = "Número de estudiantes matriculados por primera vez en postgrado por sexo - sede Bogotá", titulo = "Estudiantes de postgrado matriculados por primera vez por sexo - sede Bogotá");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por sexo - sede Bogotá", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por sexo - sede Bogotá", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---


col <-   c( "#6d6666", # gris, 25 años o menos
            "#8cc63f", # verde, 26 a 30 años
            "#0071bc", # azul vivo, 31 a 35 años
            "#f15a24", # naranja, 36 años o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados por primera vez en postgrado por grupos de edad - sede Bogotá', mensaje = "Número de matriculados por primera vez en postgrado por grupos de edad - sede Bogotá", titulo = "Matriculados en postgrado por primera vez por grupos de edad - sede Bogotá");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por grupos de edad - sede Bogotá", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por grupos de edad - sede Bogotá", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL



# Convenio ---

col <-   c( "#f15a24", # naranja, No
            "#8cc63f", # verde, Sí
            "#0071bc") # Azul vivo, Sin información


CONVENIO_TABLA <- tabla(datos = Consolidado, categoria = "CONVENIO", variable = 'Total estudiantes matriculados por primera vez en postgrado en convenios - sede Bogotá', mensaje = "Total de estudiantes matriculados por primera vez en postgrado en convenios - sede Bogotá", titulo = "Total estudiantes matriculados por primera vez en postgrado en convenios - sede Bogotá");CONVENIO_TABLA
CONVENIO_SERIE <- series(datos = Consolidado, categoria = "CONVENIO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado en convenios - sede Bogotá", eje = "Número de estudiantes (k: miles)");CONVENIO_SERIE
CONVENIO_ACTUAL <- drilldown_si_no_torta(datos = Consolidado, categoria = "CONVENIO", categoria_drilldown = "TIP_CONVENIO", colores = col, titulo = "Distribución de matriculados por primera vez en postgrado en convenios - sede Bogotá, periodo", titulo_drilldown = "Matriculados", etiqueta = "Total de matriculados", eje = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);CONVENIO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales  


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Bogotá', mensaje = "Total de estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Bogotá", titulo = "Sede estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Bogotá");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Bog/Mpvpos", "Serie.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Bog/Mpvpos", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Bog/Mpvpos", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Bog/Mpvpos", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Bog/Mpvpos", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Bog/Mpvpos", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Bog/Mpvpos", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Bog/Mpvpos", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Bog/Mpvpos", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Bog/Mpvpos", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Bog/Mpvpos", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Bog/Mpvpos", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Bog/Mpvpos", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Bog/Mpvpos", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Bog/Mpvpos", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Bog/Mpvpos", "A_edad.html")
Salvar(CONVENIO_TABLA, "G_Matriculados/Bog/Mpvpos", "T_convenio.html")
Salvar(CONVENIO_SERIE, "G_Matriculados/Bog/Mpvpos", "S_convenio.html")
Salvar(CONVENIO_ACTUAL, "G_Matriculados/Bog/Mpvpos", "A_convenio.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Bog/Mpvpos", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Bog/Mpvpos", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Bog/Mpvpos", "A_snies.html")



# Mt_Pvez_Pos1102 ----


# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPvezPos1102") %>% select(-(Nivel))


# Evolución histórica---


col <-   c("#f15a24") # Naranja, Total sede Medellín


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados por primera vez en postgrado - sede Medellín", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d", # rojo, Maestría
            "#fbb03b") # amarillo, Especialidades médicas

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados por primera vez en postgrado por nivel de formación - sede Medellín', mensaje = "Número de estudiantes matriculados por primera vez en postgrado por nivel de formación - sede Medellín", titulo = "Matriculados por primera vez en postgrado por nivel de formación, sede Medellín");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por nivel de formación - sede Medellín", eje = "Número de matriculados");NIVEL_SERIE
NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "NIVEL", colores = col, titulo = "Distribución de matriculados por primera vez en postgrado por nivel de formación - sede Medellín", etiqueta = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NIVEL_ACTUAL


# Facultad ---

col <-   c( "#9e9ac8",  # Morado claro, Arquitectura
            "#f15a24", # naranja, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias humanas y económicas 
            "#8cc63f" # verde, Minas
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados por primera vez en postgrado por facultad - sede Medellín', mensaje = "Total de estudiantes matriculados por primera vez en postgrado por facultad - sede Medellín", titulo = "Facultad estudiantes matriculados por primera vez en postgrado - sede Medellín");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por facultad - sede Medellín", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por facultad - sede Medellín", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL



# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información

NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados por primera vez en postgrado según nacionalidad - sede Medellín', mensaje = "Número de estudiantes matriculados por primera vez en postgrado por nacionalidad - sede Medellín", titulo = "Matriculados por primera vez en postgrado según nacionalidad - sede Medellín");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado según nacionalidad - sede Medellín", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado según la nacionalidad - sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por primera vez en postgrado por sexo - sede Medellín', mensaje = "Número de estudiantes matriculados por primera vez en postgrado por sexo - sede Medellín", titulo = "Estudiantes de postgrado matriculados por primera vez por sexo - sede Medellín");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por sexo - sede Medellín", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por sexo - sede Medellín", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---


col <-   c( "#6d6666", # gris, 25 años o menos
            "#8cc63f", # verde, 26 a 30 años
            "#0071bc", # azul vivo, 31 a 35 años
            "#f15a24", # naranja, 36 años o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados por primera vez en postgrado por grupos de edad - sede Medellín', mensaje = "Número de matriculados por primera vez en postgrado por grupos de edad - sede Medellín", titulo = "Matriculados en postgrado por primera vez por grupos de edad - sede Medellín");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por grupos de edad - sede Medellín", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por grupos de edad - sede Medellín", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL



# Convenio ---

col <-   c( "#f15a24", # naranja, No
            "#8cc63f", # verde, Sí
            "#0071bc") # Azul vivo, Sin información


CONVENIO_TABLA <- tabla(datos = Consolidado, categoria = "CONVENIO", variable = 'Total estudiantes matriculados por primera vez en postgrado en convenios - sede Medellín', mensaje = "Total de estudiantes matriculados por primera vez en postgrado en convenios - sede Medellín", titulo = "Total estudiantes matriculados por primera vez en postgrado en convenios - sede Medellín");CONVENIO_TABLA
CONVENIO_SERIE <- series(datos = Consolidado, categoria = "CONVENIO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado en convenios - sede Medellín", eje = "Número de estudiantes (k: miles)");CONVENIO_SERIE
CONVENIO_ACTUAL <- drilldown_si_no_torta(datos = Consolidado, categoria = "CONVENIO", categoria_drilldown = "TIP_CONVENIO", colores = col, titulo = "Distribución de matriculados por primera vez en postgrado en convenios - sede Medellín, periodo", titulo_drilldown = "Matriculados", etiqueta = "Total de matriculados", eje = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);CONVENIO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales  


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Medellín', mensaje = "Total de estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Medellín", titulo = "Sede estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Medellín");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Medellín", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Medellín", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Matriculados/Med/Mpvpos", "Serie.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Med/Mpvpos", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Med/Mpvpos", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Med/Mpvpos", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Med/Mpvpos", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Med/Mpvpos", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Med/Mpvpos", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Med/Mpvpos", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Med/Mpvpos", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Med/Mpvpos", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Med/Mpvpos", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Med/Mpvpos", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Med/Mpvpos", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Med/Mpvpos", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Med/Mpvpos", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Med/Mpvpos", "A_edad.html")
Salvar(CONVENIO_TABLA, "G_Matriculados/Med/Mpvpos", "T_convenio.html")
Salvar(CONVENIO_SERIE, "G_Matriculados/Med/Mpvpos", "S_convenio.html")
Salvar(CONVENIO_ACTUAL, "G_Matriculados/Med/Mpvpos", "A_convenio.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Med/Mpvpos", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Med/Mpvpos", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Med/Mpvpos", "A_snies.html")


# Mt_Pvez_Pos1103 ----


# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPvezPos1103") %>% select(-(Nivel))


# Evolución histórica---


col <-   c("#0071bc") # Azul vivo, Total sede Manizales


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados por primera vez en postgrado - sede Manizales", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d", # rojo, Maestría
            "#fbb03b") # amarillo, Especialidades médicas

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados por primera vez en postgrado por nivel de formación - sede Manizales', mensaje = "Número de estudiantes matriculados por primera vez en postgrado por nivel de formación - sede Manizales", titulo = "Matriculados por primera vez en postgrado por nivel de formación, sede Manizales");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por nivel de formación - sede Manizales", eje = "Número de matriculados");NIVEL_SERIE
NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "NIVEL", colores = col, titulo = "Distribución de matriculados por primera vez en postgrado por nivel de formación - sede Manizales", etiqueta = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NIVEL_ACTUAL


# Facultad ---

col <-   c( "#f15a24", # naranja, Administración
            "#0071bc", # azul vivo, Ciencias exactas y naturales
            "#8cc63f" # verde, Ingeniaría y arquitectura
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados por primera vez en postgrado por facultad - sede Manizales', mensaje = "Total de estudiantes matriculados por primera vez en postgrado por facultad - sede Manizales", titulo = "Facultad estudiantes matriculados por primera vez en postgrado - sede Manizales");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por facultad - sede Manizales", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por facultad - sede Manizales", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL



# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información

NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados por primera vez en postgrado según nacionalidad - sede Manizales', mensaje = "Número de estudiantes matriculados por primera vez en postgrado por nacionalidad - sede Manizales", titulo = "Matriculados por primera vez en postgrado según nacionalidad - sede Manizales");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado según nacionalidad - sede Manizales", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado según la nacionalidad - sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por primera vez en postgrado por sexo - sede Manizales', mensaje = "Número de estudiantes matriculados por primera vez en postgrado por sexo - sede Manizales", titulo = "Estudiantes de postgrado matriculados por primera vez por sexo - sede Manizales");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por sexo - sede Manizales", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por sexo - sede Manizales", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---


col <-   c( "#6d6666", # gris, 25 años o menos
            "#8cc63f", # verde, 26 a 30 años
            "#0071bc", # azul vivo, 31 a 35 años
            "#f15a24", # naranja, 36 años o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados por primera vez en postgrado por grupos de edad - sede Manizales', mensaje = "Número de matriculados por primera vez en postgrado por grupos de edad - sede Manizales", titulo = "Matriculados en postgrado por primera vez por grupos de edad - sede Manizales");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por grupos de edad - sede Manizales", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por grupos de edad - sede Manizales", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL



# Convenio ---

col <-   c( "#f15a24", # naranja, No
            "#8cc63f", # verde, Sí
            "#0071bc") # Azul vivo, Sin información


CONVENIO_TABLA <- tabla(datos = Consolidado, categoria = "CONVENIO", variable = 'Total estudiantes matriculados por primera vez en postgrado en convenios - sede Manizales', mensaje = "Total de estudiantes matriculados por primera vez en postgrado en convenios - sede Manizales", titulo = "Total estudiantes matriculados por primera vez en postgrado en convenios - sede Manizales");CONVENIO_TABLA
CONVENIO_SERIE <- series(datos = Consolidado, categoria = "CONVENIO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado en convenios - sede Manizales", eje = "Número de estudiantes (k: miles)");CONVENIO_SERIE
CONVENIO_ACTUAL <- drilldown_si_no_torta(datos = Consolidado, categoria = "CONVENIO", categoria_drilldown = "TIP_CONVENIO", colores = col, titulo = "Distribución de matriculados por primera vez en postgrado en convenios - sede Manizales, periodo", titulo_drilldown = "Matriculados", etiqueta = "Total de matriculados", eje = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);CONVENIO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales  


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Manizales', mensaje = "Total de estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Manizales", titulo = "Sede estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Manizales");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Manizales", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Manizales", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Matriculados/Man/Mpvpos", "Serie.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Man/Mpvpos", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Man/Mpvpos", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Man/Mpvpos", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Man/Mpvpos", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Man/Mpvpos", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Man/Mpvpos", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Man/Mpvpos", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Man/Mpvpos", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Man/Mpvpos", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Man/Mpvpos", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Man/Mpvpos", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Man/Mpvpos", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Man/Mpvpos", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Man/Mpvpos", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Man/Mpvpos", "A_edad.html")
Salvar(CONVENIO_TABLA, "G_Matriculados/Man/Mpvpos", "T_convenio.html")
Salvar(CONVENIO_SERIE, "G_Matriculados/Man/Mpvpos", "S_convenio.html")
Salvar(CONVENIO_ACTUAL, "G_Matriculados/Man/Mpvpos", "A_convenio.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Man/Mpvpos", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Man/Mpvpos", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Man/Mpvpos", "A_snies.html")


# Mt_Pvez_Pos1104 ----


# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "MtPvezPos1104") %>% select(-(Nivel))


# Evolución histórica---


col <-   c("#93278f") # Morado, Total sede Palmira


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes matriculados por primera vez en postgrado - sede Palmira", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d", # rojo, Maestría
            "#fbb03b") # amarillo, Especialidades médicas

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes matriculados por primera vez en postgrado por nivel de formación - sede Palmira', mensaje = "Número de estudiantes matriculados por primera vez en postgrado por nivel de formación - sede Palmira", titulo = "Matriculados por primera vez en postgrado por nivel de formación, sede Palmira");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por nivel de formación - sede Palmira", eje = "Número de matriculados");NIVEL_SERIE
NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "NIVEL", colores = col, titulo = "Distribución de matriculados por primera vez en postgrado por nivel de formación - sede Palmira", etiqueta = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NIVEL_ACTUAL


# Facultad ---

col <-   c( "#0071bc", # azul vivo, Ciencias agropecuarias 
            "#8cc63f" # verde, Ingeniería y Administración
)


FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes matriculados por primera vez en postgrado por facultad - sede Palmira', mensaje = "Total de estudiantes matriculados por primera vez en postgrado por facultad - sede Palmira", titulo = "Facultad estudiantes matriculados por primera vez en postgrado - sede Palmira");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por facultad - sede Palmira", eje = "Número de estudiantes (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por facultad - sede Palmira", eje = "Número de estudiantes (k: miles)"); FACULTAD_ACTUAL



# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información

NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes matriculados por primera vez en postgrado según nacionalidad - sede Palmira', mensaje = "Número de estudiantes matriculados por primera vez en postgrado por nacionalidad - sede Palmira", titulo = "Matriculados por primera vez en postgrado según nacionalidad - sede Palmira");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado según nacionalidad - sede Palmira", eje = "Número de estudiantes  (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado según la nacionalidad - sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total estudiantes matriculados por primera vez en postgrado por sexo - sede Palmira', mensaje = "Número de estudiantes matriculados por primera vez en postgrado por sexo - sede Palmira", titulo = "Estudiantes de postgrado matriculados por primera vez por sexo - sede Palmira");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por sexo - sede Palmira", eje = "Número de estudiantes (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por sexo - sede Palmira", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---


col <-   c( "#6d6666", # gris, 25 años o menos
            "#8cc63f", # verde, 26 a 30 años
            "#0071bc", # azul vivo, 31 a 35 años
            "#f15a24", # naranja, 36 años o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total estudiantes matriculados por primera vez en postgrado por grupos de edad - sede Palmira', mensaje = "Número de matriculados por primera vez en postgrado por grupos de edad - sede Palmira", titulo = "Matriculados en postgrado por primera vez por grupos de edad - sede Palmira");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por grupos de edad - sede Palmira", eje = "Número de estudiantes (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por grupos de edad - sede Palmira", eje = "Número de estudiantes (k: miles)"); CAT_EDAD_ACTUAL



# Convenio ---

col <-   c( "#f15a24", # naranja, No
            "#8cc63f", # verde, Sí
            "#0071bc") # Azul vivo, Sin información


CONVENIO_TABLA <- tabla(datos = Consolidado, categoria = "CONVENIO", variable = 'Total estudiantes matriculados por primera vez en postgrado en convenios - sede Palmira', mensaje = "Total de estudiantes matriculados por primera vez en postgrado en convenios - sede Palmira", titulo = "Total estudiantes matriculados por primera vez en postgrado en convenios - sede Palmira");CONVENIO_TABLA
CONVENIO_SERIE <- series(datos = Consolidado, categoria = "CONVENIO", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado en convenios - sede Palmira", eje = "Número de estudiantes (k: miles)");CONVENIO_SERIE
CONVENIO_ACTUAL <- drilldown_si_no_torta(datos = Consolidado, categoria = "CONVENIO", categoria_drilldown = "TIP_CONVENIO", colores = col, titulo = "Distribución de matriculados por primera vez en postgrado en convenios - sede Palmira, periodo", titulo_drilldown = "Matriculados", etiqueta = "Total de matriculados", eje = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);CONVENIO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales  


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Palmira', mensaje = "Total de estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Palmira", titulo = "Sede estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Palmira");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Palmira", eje = "Número de estudiantes (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados por primera vez en postgrado por áreas del conocimiento SNIES - sede Palmira", eje = "Número de estudiantes"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Matriculados/Pal/Mpvpos", "Serie.html")
Salvar(NIVEL_TABLA, "G_Matriculados/Pal/Mpvpos", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Matriculados/Pal/Mpvpos", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Matriculados/Pal/Mpvpos", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Matriculados/Pal/Mpvpos", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Matriculados/Pal/Mpvpos", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Matriculados/Pal/Mpvpos", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Matriculados/Pal/Mpvpos", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Matriculados/Pal/Mpvpos", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Matriculados/Pal/Mpvpos", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Matriculados/Pal/Mpvpos", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Matriculados/Pal/Mpvpos", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Matriculados/Pal/Mpvpos", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Matriculados/Pal/Mpvpos", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Matriculados/Pal/Mpvpos", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Matriculados/Pal/Mpvpos", "A_edad.html")
Salvar(CONVENIO_TABLA, "G_Matriculados/Pal/Mpvpos", "T_convenio.html")
Salvar(CONVENIO_SERIE, "G_Matriculados/Pal/Mpvpos", "S_convenio.html")
Salvar(CONVENIO_ACTUAL, "G_Matriculados/Pal/Mpvpos", "A_convenio.html")
Salvar(AREAC_SNIES_TABLA, "G_Matriculados/Pal/Mpvpos", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Matriculados/Pal/Mpvpos", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Matriculados/Pal/Mpvpos", "A_snies.html")





