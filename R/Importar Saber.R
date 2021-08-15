# Librerías ----

library("googledrive")
library(tidyverse) # version 1.2.1
library(readxl)    # version 1.0.0
library(dplyr)

options(digits = 10)

# Funciones ----

# IMPORTAR MICRODATOS

importar <- function(Archivo, Periodo){
  read_excel(Archivo, sheet = Periodo, guess_max = 100000,  col_types = tipovar)}


# FUNCIÓN DE AGREGACIÓN POR DESAGREGACIONES TEMÁTICAS

Agregar <- function(poblacion, var){
  poblacion %>% group_by(.dots = c("YEAR", "SEMESTRE", var), .drop = FALSE) %>% 
    summarise(Total = n()) %>% 
    rename("Clase"=var) %>% 
    mutate(Variable = var) %>%
    select(Variable, YEAR, SEMESTRE, Clase, Total) %>%
    ungroup()
}


# FUNCIÓN DE TOTALES POR DESAGREGACIÓN TEMÁTICA

Totales <- function(poblacion){
  poblacion %>% group_by(YEAR, SEMESTRE, .drop = FALSE) %>%  summarise(Total = n()) %>% ungroup() %>%
    mutate(Variable="TOTAL", YEAR=YEAR, SEMESTRE=SEMESTRE, Clase = "Total", Total=Total) %>%
    select(Variable, YEAR, SEMESTRE, Clase, Total)
}

# Importar Microdatos ----

drive_download("Estadisticas UN/Matriculados/P2009 Matricula.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Matriculados/P2010 Matricula.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Matriculados/P2011 Matricula.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Matriculados/P2012 Matricula.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Matriculados/P2013 Matricula.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Matriculados/P2014 Matricula.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Matriculados/P2015 Matricula.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Matriculados/P2016 Matricula.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Matriculados/P2017 Matricula.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Matriculados/P20181 Matricula.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Matriculados/P20182 Matricula.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Matriculados/P20191 Matricula.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Matriculados/P20192 Matricula.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Matriculados/P20201 Matricula.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Matriculados/P20202 Matricula.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Matriculados/P20211 Matricula.xlsx", overwrite = TRUE)

# Definir tipo de las variables a importar

tipovar <- c("text", "text", "text", "text", "text", "text", "text", "numeric", 
             "text", "numeric", "text", "text", "text", "numeric", "text", "numeric", 
             "text", "text", "text", "numeric", "text", "numeric", "text", "text", 
             "text", "text", "text", "numeric", "text", "text", "text", "text", "numeric", 
             "text", "numeric", "text", "text", "text", "text", "text", "text", "text", 
             "text", "text", "numeric", "text", "text", "text", "numeric", "text", "text", 
             "text", "numeric", "numeric", "text")

# Importar

M2009 <- importar("P2009 Matricula.xlsx", "2009 Matrícula") 
M2010 <- importar("P2010 Matricula.xlsx", "2010 Matrícula") 
M2011 <- importar("P2011 Matricula.xlsx", "2011 Matrícula") 
M2012 <- importar("P2012 Matricula.xlsx", "2012 Matrícula") 
M2013 <- importar("P2013 Matricula.xlsx", "2013 Matrícula") 
M2014 <- importar("P2014 Matricula.xlsx", "2014 Matrícula") 
M2015 <- importar("P2015 Matricula.xlsx", "2015 Matrícula") 
M2016 <- importar("P2016 Matricula.xlsx", "2016 Matrícula") 
M2017 <- importar("P2017 Matricula.xlsx", "2017 Matrícula") 
M20181 <- importar("P20181 Matricula.xlsx", "20181 Matrícula")
M20182 <- importar("P20182 Matricula.xlsx", "20182 Matrícula") 
M20191 <- importar("P20191 Matricula.xlsx", "20191 Matrícula") 
M20192 <- importar("P20192 Matricula.xlsx", "20192 Matrícula")
M20201 <- importar("P20201 Matricula.xlsx", "20201 Matrícula") 
M20202 <- importar("P20202 Matricula.xlsx", "P2020-2 Matrícula") 
M20211 <- importar("P20211 Matricula.xlsx", "P2021-1 Matrícula") 

Microdatos <- bind_rows(M2009, M2010, M2011, M2012, M2013, M2014, M2015, M2016, M2017, M20181, 
                        M20182, M20191, M20192, M20201, M20202, M20211)


Microdatos$LON_CIU_NAC	<- parse_number(str_replace(Microdatos$LON_CIU_NAC, ",", "."))
Microdatos$LAT_CIU_NAC <- parse_number(str_replace(Microdatos$LAT_CIU_NAC, ",", "."))
Microdatos$LON_CIU_PROC <- parse_number(str_replace(Microdatos$LON_CIU_PROC, ",", "."))
Microdatos$LAT_CIU_PROC <- parse_number(str_replace(Microdatos$LAT_CIU_PROC, ",", "."))

unlink(c("P2009 Matricula.xlsx", "P2010 Matricula.xlsx", "P2011 Matricula.xlsx",
         "P2012 Matricula.xlsx", "P2013 Matricula.xlsx", "P2014 Matricula.xlsx",
         "P2015 Matricula.xlsx", "P2016 Matricula.xlsx", "P2017 Matricula.xlsx",
         "P20181 Matricula.xlsx", "P20182 Matricula.xlsx", "P20191 Matricula.xlsx",
         "P20192 Matricula.xlsx", "P20201 Matricula.xlsx", "P20202 Matricula.xlsx",
         "P20211 Matricula.xlsx"))


# Transformaciones ----


Microdatos <- Microdatos %>% mutate(TIPO_NIVEL = if_else(is.na(TIPO_NIVEL), "Sin información", TIPO_NIVEL),
                                    NIVEL = if_else(is.na(NIVEL), "Sin información", NIVEL),
                                    SEDE_NOMBRE_MAT = if_else(is.na(SEDE_NOMBRE_MAT), "Sin información", SEDE_NOMBRE_MAT),
                                    SEDE_NOMBRE_MAT = if_else(SEDE_NOMBRE_MAT == "De La Paz", "La Paz", SEDE_NOMBRE_MAT),
                                    FACULTAD = if_else(FACULTAD %in% c("Agronomía", "Ciencias grarias"), "Ciencias agrarias", FACULTAD),
                                    FACULTAD = if_else(FACULTAD %in% c("Ingenieria"), "Ingeniería", FACULTAD),
                                    FACULTAD = if_else(FACULTAD %in% c("Ciencias humanas  y económicas"), "Ciencias humanas y económicas", FACULTAD),
                                    FACULTAD = if_else(FACULTAD %in% c("Ciencias agropecuarias") & SEDE_NOMBRE_MAT == "Medellín" , "Ciencias agrarias", FACULTAD),
                                    FACULTAD = if_else(FACULTAD %in% c("Ingenieria y administración"), "Ingeniería y administración", FACULTAD),
                                    NACIONALIDAD = if_else(is.na(NACIONALIDAD), "Sin información", NACIONALIDAD),
                                    SEXO = if_else(is.na(SEXO), "Sin información", SEXO),
                                    CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD),
                                    ESTRATO = if_else(is.na(ESTRATO), "Sin información", ESTRATO),
                                    TIPO_COL = if_else(is.na(TIPO_COL), "Sin información", TIPO_COL),
                                    PBM = if_else(is.na(PBM), "Sin información", PBM),
                                    MAT_PVEZ = if_else(is.na(MAT_PVEZ), "Sin información", MAT_PVEZ),
                                    MOD_ADM = if_else(is.na(MOD_ADM), "Sin información", MOD_ADM),
                                    TIPO_ADM = if_else(is.na(TIPO_ADM), "Sin información", TIPO_ADM),
                                    PAES = if_else(is.na(PAES), "Sin información", PAES),
                                    PEAMA = if_else(is.na(PEAMA), "Sin información", PEAMA),
                                    MOV_PEAMA = if_else(is.na(MOV_PEAMA), "Sin información", MOV_PEAMA),
                                    CONVENIO = if_else(is.na(CONVENIO), "Sin información", CONVENIO),
                                    TIP_CONVENIO = if_else(is.na(TIP_CONVENIO), "Sin información", TIP_CONVENIO),
                                    AREAC_SNIES = if_else(is.na(AREAC_SNIES), "Sin información", AREAC_SNIES),
                                    NIVEL = if_else(NIVEL == "Especialidades  médicas", "Especialidades médicas", NIVEL),
                                    CAT_EDAD = if_else(CAT_EDAD == "26 o  más años", "26 o más años", CAT_EDAD)
                                    )

# CREAR FACTORES A PARTIR DE VARIABLES CUALITATIVAS

Microdatos$YEAR <- factor(Microdatos$YEAR, levels = c(2009:2021))
Microdatos$SEMESTRE <- factor(Microdatos$SEMESTRE, levels = c(1, 2))
Microdatos$TIPO_NIVEL <- factor(Microdatos$TIPO_NIVEL, levels = c('Postgrado', 'Pregrado'))
# Microdatos$NIVEL <- factor(Microdatos$NIVEL, levels = c('Doctorado', 'Especialidades médicas', 'Especialización', 'Maestría', 'Pregrado', 'Tecnología'))
Microdatos$SEDE_NOMBRE_MAT <- factor(Microdatos$SEDE_NOMBRE_MAT, levels = c('Amazonía', 'Bogotá', 'Caribe', 'La Paz', 'Manizales', 'Medellín', 'Orinoquía', 'Palmira', 'Tumaco'))
Microdatos$NACIONALIDAD <- factor(Microdatos$NACIONALIDAD, levels = c('Colombiana', 'Extranjero', 'Sin información'))
Microdatos$SEXO <- factor(Microdatos$SEXO, levels = c('Hombres', 'Mujeres'))
Microdatos$ESTRATO <- factor(Microdatos$ESTRATO, levels = c('Estrato 2 o menos', 'Estrato 3', 'Estrato 4 o más', 'ND/NE'))
Microdatos$TIPO_COL <- factor(Microdatos$TIPO_COL, levels = c('Oficial', 'Otros', 'Privado', 'Sin información'))
Microdatos$PBM <- factor(Microdatos$PBM, levels = c('11 o menos', '12 a 17', '18 a 50', '51 a 100', 'Sin información'))
Microdatos$MAT_PVEZ <- factor(Microdatos$MAT_PVEZ, levels = c('No', 'Sí'))
Microdatos$MOD_ADM <- factor(Microdatos$MOD_ADM, levels = c('Especial', 'Regular'))
# Microdatos$TIPO_ADM <- factor(Microdatos$TIPO_ADM, levels = c('PAES', 'PEAA', 'PEAMA', 'Regular'))
# Microdatos$PAES <- factor(Microdatos$PAES, levels = c('Comunidades indígenas', 'Mejores bachilleres', 'Mejores bachilleres de municipios pobres', 'Población afrocolombiana', 'Victimas del conflicto armado interno en Colombia'))
Microdatos$PEAMA <- factor(Microdatos$PEAMA, levels = c('PEAMA - Amazonía', 'PEAMA - Caribe', 'PEAMA - Medellín - Sinifaná', 'PEAMA - Orinoquía', 'PEAMA - Sede Bogotá - Sumapaz', 'PEAMA - Sede Manizales - Caldas', 'PEAMA - Tumaco'))
Microdatos$MOV_PEAMA <- factor(Microdatos$MOV_PEAMA, levels = c('Etapa de movilidad', 'Etapa Inicial'))
Microdatos$CONVENIO <- factor(Microdatos$CONVENIO, levels = c('No', 'Sí', 'Sin información'))
Microdatos$TIP_CONVENIO <- factor(Microdatos$TIP_CONVENIO, levels = c('Externo', 'Interno', 'Sin información'))
Microdatos$AREAC_SNIES <- factor(Microdatos$AREAC_SNIES, levels = c('Agronomía, veterinaria y afines', 'Bellas artes', 'Ciencias de la educación','Ciencias de la salud', 'Ciencias sociales y humanas', 'Economía, administración, contaduría y afines', 'Ingeniería, arquitectura, urbanismo y afines', 'Matemáticas y ciencias naturales'))


# Poblaciones ----

# Matriculados

Mat_Nacional <- Microdatos # Total matriculados a nivel nacional
Mat_Bogota <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Bogotá") # Total matriculados Bogotá
Mat_Medellin <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Medellín") # Total matriculados Medellín
Mat_Manizales <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Manizales") # Total matriculados Manizales
Mat_Palmira <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Palmira") # Total matriculados Palmira

# Matriculados en pregrado
Mat_Pre_Nacional <- Microdatos %>% filter(TIPO_NIVEL == "Pregrado") # Total matriculados pregrado a nivel nacional
Mat_Pre_Bogota <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Bogotá", TIPO_NIVEL == "Pregrado") # Total matriculados pregrado Bogotá
Mat_Pre_Medellin <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Medellín", TIPO_NIVEL == "Pregrado") # Total matriculados pregrado Medellín
Mat_Pre_Manizales <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Manizales", TIPO_NIVEL == "Pregrado") # Total matriculados pregrado Manizales
Mat_Pre_Palmira <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Palmira", TIPO_NIVEL == "Pregrado") # Total matriculados pregrado Palmira


# Matriculados en postgrado

Mat_Pos_Nacional <- Microdatos %>% filter(TIPO_NIVEL == "Postgrado") # Total matriculados postgrado a nivel nacional
Mat_Pos_Bogota <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Bogotá", TIPO_NIVEL == "Postgrado") # Total matriculados postgrado Bogotá
Mat_Pos_Medellin <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Medellín", TIPO_NIVEL == "Postgrado") # Total matriculados postgrado Medellín
Mat_Pos_Manizales <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Manizales", TIPO_NIVEL == "Postgrado") # Total matriculados postgrado Manizales
Mat_Pos_Palmira <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Palmira", TIPO_NIVEL == "Postgrado") # Total matriculados postgrado Palmira


# Matriculados primera vez


Mat_pvez_Nacional <- Microdatos %>% filter(MAT_PVEZ == "Sí") # Total matriculados primera vez Nacional
Mat_pvez_Bogota <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Bogotá", MAT_PVEZ == "Sí") # Total matriculados primera vez Bogotá
Mat_pvez_Medellin <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Medellín", MAT_PVEZ == "Sí") # Total matriculados primera vez Medellín
Mat_pvez_Manizales <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Manizales", MAT_PVEZ == "Sí") # Total matriculados primera vez Manizales
Mat_pvez_Palmira <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Palmira", MAT_PVEZ == "Sí") # Total matriculados primera vez Palmira

# Matriculados primera vez en pregrado

Mat_pvez_pre_Nacional <- Microdatos %>% filter(MAT_PVEZ == "Sí", TIPO_NIVEL == "Pregrado") # Total matriculados en pregrado primera vez Nacional
Mat_pvez_pre_Bogota <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Bogotá", MAT_PVEZ == "Sí", TIPO_NIVEL == "Pregrado") # Total matriculados en pregrado primera vez Bogotá
Mat_pvez_pre_Medellin <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Medellín", MAT_PVEZ == "Sí", TIPO_NIVEL == "Pregrado") # Total matriculados en pregrado primera vez Medellín
Mat_pvez_pre_Manizales <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Manizales", MAT_PVEZ == "Sí", TIPO_NIVEL == "Pregrado") # Total matriculados en pregrado primera vez Manizales
Mat_pvez_pre_Palmira <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Palmira", MAT_PVEZ == "Sí", TIPO_NIVEL == "Pregrado") # Total matriculados en pregrado primera vez Palmira

# Matriculados primera vez en postgrado

Mat_pvez_pos_Nacional <- Microdatos %>% filter(MAT_PVEZ == "Sí", TIPO_NIVEL == "Postgrado") # Total matriculados en pregrado primera vez Nacional
Mat_pvez_pos_Bogota <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Bogotá", MAT_PVEZ == "Sí", TIPO_NIVEL == "Postgrado") # Total matriculados en pregrado primera vez Bogotá
Mat_pvez_pos_Medellin <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Medellín", MAT_PVEZ == "Sí", TIPO_NIVEL == "Postgrado") # Total matriculados en pregrado primera vez Medellín
Mat_pvez_pos_Manizales <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Manizales", MAT_PVEZ == "Sí", TIPO_NIVEL == "Postgrado") # Total matriculados en pregrado primera vez Manizales
Mat_pvez_pos_Palmira <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Palmira", MAT_PVEZ == "Sí", TIPO_NIVEL == "Postgrado") # Total matriculados en pregrado primera vez Palmira


# MATRICULA 1100 ---- 

# Tabla agregada

DT1 <- Agregar(Mat_Nacional, 'TIPO_NIVEL')
DT2 <- Agregar(Mat_Nacional, 'NIVEL')
DT3 <- Agregar(Mat_Nacional, 'SEDE_NOMBRE_MAT')
DT3 <- mutate(DT3, Total = ifelse(Clase == "Tumaco" & YEAR %in% c(2009:2014), NA, Total))
DT3 <- mutate(DT3, Total = ifelse(Clase == "La Paz" & YEAR %in% c(2009:2018) | Clase == "La Paz" & YEAR==2019 & SEMESTRE==1, NA, Total))
DT4 <- Agregar(Mat_Nacional, 'NACIONALIDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT5 <- Agregar(Mat_Nacional, 'SEXO')
DT6 <- Agregar(Mat_Nacional, 'MAT_PVEZ')
DT7 <- Agregar(Mat_Nacional, 'AREAC_SNIES')
Total <- Totales(Mat_Nacional)


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_Nacional %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados Universidad

Mt1100 <- Agregado %>% 
               unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
               mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
               select(-Id) %>% mutate(Nivel = "Mt1100")


# Mat Bogotá 1101 ---- 

# Tabla agregada

DT1 <- Agregar(Mat_Bogota, 'TIPO_NIVEL')
DT2 <- Agregar(Mat_Bogota, 'NIVEL')
DT3 <- Agregar(Mat_Bogota, 'FACULTAD')
DT4 <- Agregar(Mat_Bogota, 'NACIONALIDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT5 <- Agregar(Mat_Bogota, 'SEXO')
DT6 <- Agregar(Mat_Bogota, 'MAT_PVEZ')
DT7 <- Agregar(Mat_Bogota, 'AREAC_SNIES')
Total <- Totales(Mat_Bogota)


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_Bogota %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

Mt1101 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "Mt1101")


# Mat Medellín 1102 ---- 

# Tabla agregada

DT1 <- Agregar(Mat_Medellin, 'TIPO_NIVEL')
DT2 <- Agregar(Mat_Medellin, 'NIVEL')
DT3 <- Agregar(Mat_Medellin, 'FACULTAD')
DT4 <- Agregar(Mat_Medellin, 'NACIONALIDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT5 <- Agregar(Mat_Medellin, 'SEXO')
DT6 <- Agregar(Mat_Medellin, 'MAT_PVEZ')
DT7 <- Agregar(Mat_Medellin, 'AREAC_SNIES')
Total <- Totales(Mat_Medellin)


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_Medellin %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados Universidad

Mt1102 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "Mt1102")

# Mat Manizales 1103 ---- 

# Tabla agregada

DT1 <- Agregar(Mat_Manizales, 'TIPO_NIVEL')
DT2 <- Agregar(Mat_Manizales, 'NIVEL')
DT3 <- Agregar(Mat_Manizales, 'FACULTAD')
DT4 <- Agregar(Mat_Manizales, 'NACIONALIDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT5 <- Agregar(Mat_Manizales, 'SEXO')
DT6 <- Agregar(Mat_Manizales, 'MAT_PVEZ')
DT7 <- Agregar(Mat_Manizales, 'AREAC_SNIES')
Total <- Totales(Mat_Manizales)


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_Manizales %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados 

Mt1103 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "Mt1103")


# Mat palmira 1104 ---- 

# Tabla agregada

DT1 <- Agregar(Mat_Palmira, 'TIPO_NIVEL')
DT2 <- Agregar(Mat_Palmira, 'NIVEL')
DT3 <- Agregar(Mat_Palmira, 'FACULTAD')
DT4 <- Agregar(Mat_Palmira, 'NACIONALIDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT5 <- Agregar(Mat_Palmira, 'SEXO')
DT6 <- Agregar(Mat_Palmira, 'MAT_PVEZ')
DT7 <- Agregar(Mat_Palmira, 'AREAC_SNIES')
Total <- Totales(Mat_Palmira)


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_Palmira %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados Universidad

Mt1104 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "Mt1104")


# MATRICULA PRE 1100 ---- 

# Modificar niveles y crear factor para edad 


Mat_Pre_Nacional <- Mat_Pre_Nacional %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_Pre_Nacional$CAT_EDAD <- factor(Mat_Pre_Nacional$CAT_EDAD, levels = c('17 años o menos', '18 a 20 años', '21 a 25 años', '26 o más años', 'Sin información'))
Mat_Pre_Nacional$PAES <- factor(Mat_Pre_Nacional$PAES, levels = c('Comunidades indígenas', 'De La Paz', 'Mejores bachilleres', 'Mejores bachilleres de municipios pobres', 'Población afrocolombiana', 'Victimas del conflicto armado interno en Colombia'))


# Tabla agregada


DT1 <- Agregar(Mat_Pre_Nacional, 'SEDE_NOMBRE_MAT')
DT1 <- mutate(DT1, Total = ifelse(Clase == "Tumaco" & YEAR %in% c(2009:2014), NA, Total))
DT1 <- mutate(DT1, Total = ifelse(Clase == "La Paz" & YEAR %in% c(2009:2018) | Clase == "La Paz" & YEAR==2019 & SEMESTRE==1, NA, Total))
DT2 <- Agregar(Mat_Pre_Nacional, 'NACIONALIDAD')
DT2 <- mutate(DT2, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT3 <- Agregar(Mat_Pre_Nacional, 'SEXO')
DT4 <- Agregar(Mat_Pre_Nacional, 'CAT_EDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT5 <- Agregar(Mat_Pre_Nacional, 'ESTRATO')
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT6 <- Agregar(Mat_Pre_Nacional, 'TIPO_COL')
DT6 <- DT6 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT7 <- Agregar(Mat_Pre_Nacional, 'PBM')
DT7 <- DT7 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT8 <- Agregar(Mat_Pre_Nacional, 'MAT_PVEZ')
DT9 <- Agregar(Mat_Pre_Nacional, 'MOD_ADM')
DT10 <- Agregar(Mat_Pre_Nacional, 'TIPO_ADM')
DT11 <- Agregar(Mat_Pre_Nacional, 'PAES')
DT11 <- DT11 %>% filter(!is.na(Clase))
DT11 <- mutate(DT11, Total = ifelse(Clase == "De La Paz" & YEAR %in% c(2009:2018) | Clase == "De La Paz" & YEAR==2019 & SEMESTRE==1, NA, Total))
DT12 <- Agregar(Mat_Pre_Nacional, 'PEAMA')
DT12 <- DT12 %>% filter(!is.na(Clase))
DT12 <- mutate(DT12, Total = ifelse(Clase == "PEAMA - Tumaco" & YEAR %in% c(2009:2014), NA, Total))
DT12 <- mutate(DT12, Total = ifelse(Clase == "PEAMA - Sede Bogotá - Sumapaz" & YEAR %in% c(2009:2016), NA, Total))
DT12 <- mutate(DT12, Total = ifelse(Clase == "PEAMA - Sede Manizales - Caldas" & YEAR %in% c(2009:2016), NA, Total))
DT12 <- mutate(DT12, Total = ifelse(Clase == "PEAMA - Medellín - Sinifaná" & YEAR %in% c(2009:2018), NA, Total))
DT13 <- Agregar(Mat_Pre_Nacional, 'MOV_PEAMA')
DT13 <- DT13 %>% filter(!is.na(Clase))
DT14 <- Agregar(Mat_Pre_Nacional, 'AREAC_SNIES')
Total <- Totales(Mat_Pre_Nacional)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, DT11, DT12, DT13, DT14, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_Nacional %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados 

MtPre1100 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPre1100")



# Mat Pre Bogotá 1101 ---- 


# Modificar niveles y crear factor para edad 


Mat_Pre_Bogota <- Mat_Pre_Bogota %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_Pre_Bogota$CAT_EDAD <- factor(Mat_Pre_Bogota$CAT_EDAD, levels = c('17 años o menos', '18 a 20 años', '21 a 25 años', '26 o más años', 'Sin información'))
Mat_Pre_Bogota$PAES <- factor(Mat_Pre_Bogota$PAES, levels = c('Comunidades indígenas', 'Mejores bachilleres', 'Mejores bachilleres de municipios pobres', 'Población afrocolombiana', 'Victimas del conflicto armado interno en Colombia'))


# Tabla agregada


DT1 <- Agregar(Mat_Pre_Bogota, 'FACULTAD')
DT2 <- Agregar(Mat_Pre_Bogota, 'NACIONALIDAD')
DT2 <- mutate(DT2, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT3 <- Agregar(Mat_Pre_Bogota, 'SEXO')
DT4 <- Agregar(Mat_Pre_Bogota, 'CAT_EDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT5 <- Agregar(Mat_Pre_Bogota, 'ESTRATO')
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT6 <- Agregar(Mat_Pre_Bogota, 'TIPO_COL')
DT6 <- DT6 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT7 <- Agregar(Mat_Pre_Bogota, 'PBM')
DT7 <- DT7 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT8 <- Agregar(Mat_Pre_Bogota, 'MAT_PVEZ')
DT9 <- Agregar(Mat_Pre_Bogota, 'MOD_ADM')
DT10 <- Agregar(Mat_Pre_Bogota, 'TIPO_ADM')
DT11 <- Agregar(Mat_Pre_Bogota, 'PAES')
DT11 <- DT11 %>% filter(!is.na(Clase))
DT12 <- Agregar(Mat_Pre_Bogota, 'PEAMA')
DT12 <- DT12 %>% filter(!(is.na(Clase) | Clase%in%c('PEAMA - Sede Manizales - Caldas', 'PEAMA - Medellín - Sinifaná')))
DT12 <- mutate(DT12, Total = ifelse(Clase == "PEAMA - Sede Bogotá - Sumapaz" & YEAR %in% c(2009:2016), NA, Total))
DT12 <- mutate(DT12, Total = ifelse(Clase == "PEAMA - Tumaco" & YEAR %in% c(2009:2014), NA, Total))
DT13 <- Agregar(Mat_Pre_Bogota, 'MOV_PEAMA')
DT13 <- DT13 %>% filter(!is.na(Clase))
DT14 <- Agregar(Mat_Pre_Bogota, 'AREAC_SNIES')
Total <- Totales(Mat_Pre_Bogota)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, DT11, DT12, DT13, DT14, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_Pre_Bogota %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

MtPre1101 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPre1101")



# Mat Pre Medellín 1102 ---- 


# Modificar niveles y crear factor para edad 

Mat_Pre_Medellin <- Mat_Pre_Medellin %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_Pre_Medellin$CAT_EDAD <- factor(Mat_Pre_Medellin$CAT_EDAD, levels = c('17 años o menos', '18 a 20 años', '21 a 25 años', '26 o más años', 'Sin información'))
Mat_Pre_Medellin$PAES <- factor(Mat_Pre_Medellin$PAES, levels = c('Comunidades indígenas', 'Mejores bachilleres', 'Mejores bachilleres de municipios pobres', 'Población afrocolombiana', 'Victimas del conflicto armado interno en Colombia'))


# Tabla agregada


DT1 <- Agregar(Mat_Pre_Medellin, 'FACULTAD')
DT2 <- Agregar(Mat_Pre_Medellin, 'NACIONALIDAD')
DT2 <- mutate(DT2, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT3 <- Agregar(Mat_Pre_Medellin, 'SEXO')
DT4 <- Agregar(Mat_Pre_Medellin, 'CAT_EDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT5 <- Agregar(Mat_Pre_Medellin, 'ESTRATO')
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT6 <- Agregar(Mat_Pre_Medellin, 'TIPO_COL')
DT6 <- DT6 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT7 <- Agregar(Mat_Pre_Medellin, 'PBM')
DT7 <- DT7 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT8 <- Agregar(Mat_Pre_Medellin, 'MAT_PVEZ')
DT9 <- Agregar(Mat_Pre_Medellin, 'MOD_ADM')
DT10 <- Agregar(Mat_Pre_Medellin, 'TIPO_ADM')
DT11 <- Agregar(Mat_Pre_Medellin, 'PAES')
DT11 <- DT11 %>% filter(!is.na(Clase))
DT12 <- Agregar(Mat_Pre_Medellin, 'PEAMA')
DT12 <- DT12 %>% filter(!(is.na(Clase) | Clase%in%c('PEAMA - Sede Manizales - Caldas', 'PEAMA - Sede Bogotá - Sumapaz')))
DT12 <- mutate(DT12, Total = ifelse(Clase == "PEAMA - Medellín - Sinifaná" & YEAR %in% c(2009:2018), NA, Total))
DT12 <- mutate(DT12, Total = ifelse(Clase == "PEAMA - Tumaco" & YEAR %in% c(2009:2014), NA, Total))
DT13 <- Agregar(Mat_Pre_Medellin, 'MOV_PEAMA')
DT13 <- DT13 %>% filter(!is.na(Clase))
DT14 <- Agregar(Mat_Pre_Medellin, 'AREAC_SNIES')
Total <- Totales(Mat_Pre_Medellin)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, DT11, DT12, DT13, DT14, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_Pre_Medellin %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

MtPre1102 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPre1102")


# Mat Pre Manizales 1103 ---- 

# Modificar niveles y crear factor para edad 

Mat_Pre_Manizales <- Mat_Pre_Manizales %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_Pre_Manizales$CAT_EDAD <- factor(Mat_Pre_Manizales$CAT_EDAD, levels = c('17 años o menos', '18 a 20 años', '21 a 25 años', '26 o más años', 'Sin información'))
Mat_Pre_Manizales$PAES <- factor(Mat_Pre_Manizales$PAES, levels = c('Comunidades indígenas', 'Mejores bachilleres', 'Mejores bachilleres de municipios pobres', 'Población afrocolombiana', 'Victimas del conflicto armado interno en Colombia'))


# Tabla agregada


DT1 <- Agregar(Mat_Pre_Manizales, 'FACULTAD')
DT2 <- Agregar(Mat_Pre_Manizales, 'NACIONALIDAD')
DT2 <- mutate(DT2, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT3 <- Agregar(Mat_Pre_Manizales, 'SEXO')
DT4 <- Agregar(Mat_Pre_Manizales, 'CAT_EDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT5 <- Agregar(Mat_Pre_Manizales, 'ESTRATO')
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT6 <- Agregar(Mat_Pre_Manizales, 'TIPO_COL')
DT6 <- DT6 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT7 <- Agregar(Mat_Pre_Manizales, 'PBM')
DT7 <- DT7 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT8 <- Agregar(Mat_Pre_Manizales, 'MAT_PVEZ')
DT9 <- Agregar(Mat_Pre_Manizales, 'MOD_ADM')
DT10 <- Agregar(Mat_Pre_Manizales, 'TIPO_ADM')
DT11 <- Agregar(Mat_Pre_Manizales, 'PAES')
DT11 <- DT11 %>% filter(!is.na(Clase))
DT12 <- Agregar(Mat_Pre_Manizales, 'PEAMA')
DT12 <- DT12 %>% filter(!(is.na(Clase) | Clase%in%c('PEAMA - Medellín - Sinifaná', 'PEAMA - Sede Bogotá - Sumapaz')))
DT12 <- mutate(DT12, Total = ifelse(Clase == "PEAMA - Sede Manizales - Caldas" & YEAR %in% c(2009:2016), NA, Total))
DT12 <- mutate(DT12, Total = ifelse(Clase == "PEAMA - Tumaco" & YEAR %in% c(2009:2014), NA, Total))
DT13 <- Agregar(Mat_Pre_Manizales, 'MOV_PEAMA')
DT13 <- DT13 %>% filter(!is.na(Clase))
DT14 <- Agregar(Mat_Pre_Manizales, 'AREAC_SNIES')
Total <- Totales(Mat_Pre_Manizales)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, DT11, DT12, DT13, DT14, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_Pre_Manizales %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

MtPre1103 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPre1103")

# Mat Pre Palmira 1104 ---- 

# Modificar niveles y crear factor para edad 

Mat_Pre_Palmira <- Mat_Pre_Palmira %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_Pre_Palmira$CAT_EDAD <- factor(Mat_Pre_Palmira$CAT_EDAD, levels = c('17 años o menos', '18 a 20 años', '21 a 25 años', '26 o más años', 'Sin información'))
Mat_Pre_Palmira$PAES <- factor(Mat_Pre_Palmira$PAES, levels = c('Comunidades indígenas', 'Mejores bachilleres', 'Mejores bachilleres de municipios pobres', 'Población afrocolombiana', 'Victimas del conflicto armado interno en Colombia'))


# Tabla agregada


DT1 <- Agregar(Mat_Pre_Palmira, 'FACULTAD')
DT2 <- Agregar(Mat_Pre_Palmira, 'NACIONALIDAD')
DT2 <- mutate(DT2, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT3 <- Agregar(Mat_Pre_Palmira, 'SEXO')
DT4 <- Agregar(Mat_Pre_Palmira, 'CAT_EDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT5 <- Agregar(Mat_Pre_Palmira, 'ESTRATO')
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT6 <- Agregar(Mat_Pre_Palmira, 'TIPO_COL')
DT6 <- DT6 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT7 <- Agregar(Mat_Pre_Palmira, 'PBM')
DT7 <- DT7 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT8 <- Agregar(Mat_Pre_Palmira, 'MAT_PVEZ')
DT9 <- Agregar(Mat_Pre_Palmira, 'MOD_ADM')
DT10 <- Agregar(Mat_Pre_Palmira, 'TIPO_ADM')
DT11 <- Agregar(Mat_Pre_Palmira, 'PAES')
DT11 <- DT11 %>% filter(!is.na(Clase))
DT12 <- Agregar(Mat_Pre_Palmira, 'PEAMA')
DT12 <- DT12 %>% filter(!(is.na(Clase) | Clase%in%c('PEAMA - Medellín - Sinifaná', 'PEAMA - Sede Bogotá - Sumapaz', 'PEAMA - Sede Manizales - Caldas')))
DT12 <- mutate(DT12, Total = ifelse(Clase == "PEAMA - Tumaco" & YEAR %in% c(2009:2014), NA, Total))
DT13 <- Agregar(Mat_Pre_Palmira, 'MOV_PEAMA')
DT13 <- DT13 %>% filter(!is.na(Clase))
DT14 <- Agregar(Mat_Pre_Palmira, 'AREAC_SNIES')
Total <- Totales(Mat_Pre_Palmira)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, DT11, DT12, DT13, DT14, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_Pre_Palmira %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

MtPre1104 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPre1104")


# MATRICULA POS 1100 ---- 

Mat_Pos_Nacional <- Mat_Pos_Nacional %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_Pos_Nacional$CAT_EDAD <- factor(Mat_Pos_Nacional$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))


# Tabla agregada

DT1 <- Agregar(Mat_Pos_Nacional, "NIVEL")
DT1 <- filter(DT1, !Clase %in% c('Pregrado', 'Tecnología'))
DT2 <- Agregar(Mat_Pos_Nacional, 'SEDE_NOMBRE_MAT')
DT2 <- DT2 %>% filter(!(Clase%in%c('La Paz', 'Orinoquía', 'Tumaco')))
DT3 <- Agregar(Mat_Pos_Nacional, "NACIONALIDAD")
DT3 <- mutate(DT3, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT4 <- Agregar(Mat_Pos_Nacional, "SEXO")
DT5 <- Agregar(Mat_Pos_Nacional, "CAT_EDAD")
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT6 <- Agregar(Mat_Pos_Nacional, "MAT_PVEZ")
DT6 <- mutate(DT6, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT7 <- Agregar(Mat_Pos_Nacional, "CONVENIO")
DT7 <- DT7 %>% filter(!YEAR %in% c(2009, 2010), !(YEAR == 2011 & SEMESTRE == 1))
DT8 <- Agregar(Mat_Pos_Nacional, "TIP_CONVENIO")
DT8 <- DT8 %>% filter(!YEAR %in% c(2009, 2010, 2011), Clase != "Sin información") 
DT9 <- Agregar(Mat_Pos_Nacional, "AREAC_SNIES")
Total <- Totales(Mat_Pos_Nacional)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_Pos_Nacional %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

MtPos1100 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPos1100")


# Mat Pos Bogotá 1101 ---- 


Mat_Pos_Bogota <- Mat_Pos_Bogota %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_Pos_Bogota$CAT_EDAD <- factor(Mat_Pos_Bogota$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))


# Tabla agregada

DT1 <- Agregar(Mat_Pos_Bogota, "NIVEL")
DT1 <- filter(DT1, !Clase %in% c('Pregrado', 'Tecnología'))
DT2 <- Agregar(Mat_Pos_Bogota, 'FACULTAD')
DT3 <- Agregar(Mat_Pos_Bogota, "NACIONALIDAD")
DT3 <- mutate(DT3, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT4 <- Agregar(Mat_Pos_Bogota, "SEXO")
DT5 <- Agregar(Mat_Pos_Bogota, "CAT_EDAD")
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT6 <- Agregar(Mat_Pos_Bogota, "MAT_PVEZ")
DT6 <- mutate(DT6, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT7 <- Agregar(Mat_Pos_Bogota, "CONVENIO")
DT7 <- DT7 %>% filter(!YEAR %in% c(2009, 2010), !(YEAR == 2011 & SEMESTRE == 1))
DT8 <- Agregar(Mat_Pos_Bogota, "TIP_CONVENIO")
DT8 <- DT8 %>% filter(!YEAR %in% c(2009, 2010, 2011), Clase != "Sin información") 
DT9 <- Agregar(Mat_Pos_Bogota, "AREAC_SNIES")
Total <- Totales(Mat_Pos_Bogota)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_Pos_Bogota %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

MtPos1101 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPos1101")


# Mat Pos Medellín 1102 ---- 


Mat_Pos_Medellin <- Mat_Pos_Medellin %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_Pos_Medellin$CAT_EDAD <- factor(Mat_Pos_Medellin$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))


# Tabla agregada

DT1 <- Agregar(Mat_Pos_Medellin, "NIVEL")
DT1 <- filter(DT1, !Clase %in% c('Pregrado', 'Tecnología'))
DT2 <- Agregar(Mat_Pos_Medellin, 'FACULTAD')
DT3 <- Agregar(Mat_Pos_Medellin, "NACIONALIDAD")
DT3 <- mutate(DT3, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT4 <- Agregar(Mat_Pos_Medellin, "SEXO")
DT5 <- Agregar(Mat_Pos_Medellin, "CAT_EDAD")
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT6 <- Agregar(Mat_Pos_Medellin, "MAT_PVEZ")
DT6 <- mutate(DT6, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT7 <- Agregar(Mat_Pos_Medellin, "CONVENIO")
DT7 <- DT7 %>% filter(!YEAR %in% c(2009, 2010), !(YEAR == 2011 & SEMESTRE == 1))
DT8 <- Agregar(Mat_Pos_Medellin, "TIP_CONVENIO")
DT8 <- DT8 %>% filter(!YEAR %in% c(2009, 2010, 2011), Clase != "Sin información") 
DT9 <- Agregar(Mat_Pos_Medellin, "AREAC_SNIES")
Total <- Totales(Mat_Pos_Medellin)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_Pos_Medellin %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

MtPos1102 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPos1102")


# Mat Pos Manizales 1103 ---- 


Mat_Pos_Manizales <- Mat_Pos_Manizales %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_Pos_Manizales$CAT_EDAD <- factor(Mat_Pos_Manizales$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))


# Tabla agregada

DT1 <- Agregar(Mat_Pos_Manizales, "NIVEL")
DT1 <- filter(DT1, !Clase %in% c('Pregrado', 'Tecnología'))
DT2 <- Agregar(Mat_Pos_Manizales, 'FACULTAD')
DT3 <- Agregar(Mat_Pos_Manizales, "NACIONALIDAD")
DT3 <- mutate(DT3, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT4 <- Agregar(Mat_Pos_Manizales, "SEXO")
DT5 <- Agregar(Mat_Pos_Manizales, "CAT_EDAD")
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT6 <- Agregar(Mat_Pos_Manizales, "MAT_PVEZ")
DT6 <- mutate(DT6, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT7 <- Agregar(Mat_Pos_Manizales, "CONVENIO")
DT7 <- DT7 %>% filter(!YEAR %in% c(2009, 2010), !(YEAR == 2011 & SEMESTRE == 1))
DT8 <- Agregar(Mat_Pos_Manizales, "TIP_CONVENIO")
DT8 <- DT8 %>% filter(!YEAR %in% c(2009, 2010, 2011), Clase != "Sin información") 
DT9 <- Agregar(Mat_Pos_Manizales, "AREAC_SNIES")
Total <- Totales(Mat_Pos_Manizales)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_Pos_Manizales %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

MtPos1103 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPos1103")


# Mat Pos Palmira 1104 ---- 


Mat_Pos_Palmira <- Mat_Pos_Palmira %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_Pos_Palmira$CAT_EDAD <- factor(Mat_Pos_Palmira$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))


# Tabla agregada

DT1 <- Agregar(Mat_Pos_Palmira, "NIVEL")
DT1 <- filter(DT1, !Clase %in% c('Pregrado', 'Tecnología'))
DT2 <- Agregar(Mat_Pos_Palmira, 'FACULTAD')
DT3 <- Agregar(Mat_Pos_Palmira, "NACIONALIDAD")
DT3 <- mutate(DT3, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT4 <- Agregar(Mat_Pos_Palmira, "SEXO")
DT5 <- Agregar(Mat_Pos_Palmira, "CAT_EDAD")
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT6 <- Agregar(Mat_Pos_Palmira, "MAT_PVEZ")
DT6 <- mutate(DT6, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT7 <- Agregar(Mat_Pos_Palmira, "CONVENIO")
DT7 <- DT7 %>% filter(!YEAR %in% c(2009, 2010), !(YEAR == 2011 & SEMESTRE == 1))
DT8 <- Agregar(Mat_Pos_Palmira, "TIP_CONVENIO")
DT8 <- DT8 %>% filter(!YEAR %in% c(2009, 2010, 2011), Clase != "Sin información") 
DT9 <- Agregar(Mat_Pos_Palmira, "AREAC_SNIES")
Total <- Totales(Mat_Pos_Palmira)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_Pos_Palmira %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

MtPos1104 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPos1104")


# MATRICULA PVEZ 1100 ---- 

# Tabla agregada

DT1 <- Agregar(Mat_pvez_Nacional, 'TIPO_NIVEL')
DT2 <- Agregar(Mat_pvez_Nacional, 'NIVEL')
DT3 <- Agregar(Mat_pvez_Nacional, 'SEDE_NOMBRE_MAT')
DT4 <- Agregar(Mat_pvez_Nacional, 'NACIONALIDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT5 <- Agregar(Mat_pvez_Nacional, 'SEXO')
DT6 <- Agregar(Mat_pvez_Nacional, 'AREAC_SNIES')
Total <- Totales(Mat_pvez_Nacional)

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_pvez_Nacional %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados Universidad

MtPvez1100 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPvez1100")


# Mat pvez Bogotá 1101 ---- 


# Tabla agregada

DT1 <- Agregar(Mat_pvez_Bogota, 'TIPO_NIVEL')
DT2 <- Agregar(Mat_pvez_Bogota, 'NIVEL')
DT3 <- Agregar(Mat_pvez_Bogota, 'FACULTAD')
DT4 <- Agregar(Mat_pvez_Bogota, 'NACIONALIDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT5 <- Agregar(Mat_pvez_Bogota, 'SEXO')
DT6 <- Agregar(Mat_pvez_Bogota, 'AREAC_SNIES')
Total <- Totales(Mat_pvez_Bogota)

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_pvez_Bogota %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados Universidad

MtPvez1101 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPvez1101")


# Mat pvez Medellín 1102 ---- 


# Tabla agregada

DT1 <- Agregar(Mat_pvez_Medellin, 'TIPO_NIVEL')
DT2 <- Agregar(Mat_pvez_Medellin, 'NIVEL')
DT3 <- Agregar(Mat_pvez_Medellin, 'FACULTAD')
DT4 <- Agregar(Mat_pvez_Medellin, 'NACIONALIDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT5 <- Agregar(Mat_pvez_Medellin, 'SEXO')
DT6 <- Agregar(Mat_pvez_Medellin, 'AREAC_SNIES')
Total <- Totales(Mat_pvez_Medellin)

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_pvez_Medellin %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados Universidad

MtPvez1102 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPvez1102")


# Mat pvez Manizales 1103 ---- 


# Tabla agregada

DT1 <- Agregar(Mat_pvez_Manizales, 'TIPO_NIVEL')
DT2 <- Agregar(Mat_pvez_Manizales, 'NIVEL')
DT3 <- Agregar(Mat_pvez_Manizales, 'FACULTAD')
DT4 <- Agregar(Mat_pvez_Manizales, 'NACIONALIDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT5 <- Agregar(Mat_pvez_Manizales, 'SEXO')
DT6 <- Agregar(Mat_pvez_Manizales, 'AREAC_SNIES')
Total <- Totales(Mat_pvez_Manizales)

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_pvez_Manizales %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados Universidad

MtPvez1103 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPvez1103")


# Mat pvez Palmira 1104 ---- 


# Tabla agregada

DT1 <- Agregar(Mat_pvez_Palmira, 'TIPO_NIVEL')
DT2 <- Agregar(Mat_pvez_Palmira, 'NIVEL')
DT3 <- Agregar(Mat_pvez_Palmira, 'FACULTAD')
DT4 <- Agregar(Mat_pvez_Palmira, 'NACIONALIDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT5 <- Agregar(Mat_pvez_Palmira, 'SEXO')
DT6 <- Agregar(Mat_pvez_Palmira, 'AREAC_SNIES')
Total <- Totales(Mat_pvez_Palmira)

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_pvez_Palmira %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados Universidad

MtPvez1104 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPvez1104")


# MATRICULA PVZ PRE 1100 ---- 


# Modificar niveles y crear factor para edad 


Mat_pvez_pre_Nacional <- Mat_pvez_pre_Nacional %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_pvez_pre_Nacional$CAT_EDAD <- factor(Mat_pvez_pre_Nacional$CAT_EDAD, levels = c('17 años o menos', '18 a 20 años', '21 a 25 años', '26 o más años', 'Sin información'))
Mat_pvez_pre_Nacional$PAES <- factor(Mat_pvez_pre_Nacional$PAES, levels = c('Comunidades indígenas', 'De La Paz', 'Mejores bachilleres', 'Mejores bachilleres de municipios pobres', 'Población afrocolombiana', 'Victimas del conflicto armado interno en Colombia'))



# Tabla agregada

DT1 <- Agregar(Mat_pvez_pre_Nacional, 'SEDE_NOMBRE_MAT')
DT2 <- Agregar(Mat_pvez_pre_Nacional, 'NACIONALIDAD')
DT2 <- mutate(DT2, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT3 <- Agregar(Mat_pvez_pre_Nacional, 'SEXO')
DT4 <- Agregar(Mat_pvez_pre_Nacional, 'CAT_EDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT5 <- Agregar(Mat_pvez_pre_Nacional, 'ESTRATO')
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT6 <- Agregar(Mat_pvez_pre_Nacional, 'TIPO_COL')
DT6 <- DT6 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT7 <- Agregar(Mat_pvez_pre_Nacional, 'PBM')
DT7 <- DT7 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT8 <- Agregar(Mat_pvez_pre_Nacional, 'MOD_ADM')
DT9 <- Agregar(Mat_pvez_pre_Nacional, 'TIPO_ADM')
DT10 <- Agregar(Mat_pvez_pre_Nacional, 'PAES')
DT10 <- DT10 %>% filter(!is.na(Clase))
DT10 <- mutate(DT10, Total = ifelse(Clase == "De La Paz" & YEAR %in% c(2009:2018) | Clase == "De La Paz" & YEAR==2019 & SEMESTRE==1, NA, Total))
DT11 <- Agregar(Mat_pvez_pre_Nacional, 'PEAMA')
DT11 <- DT11 %>% filter(!is.na(Clase))
DT12 <- Agregar(Mat_pvez_pre_Nacional, 'AREAC_SNIES')
Total <- Totales(Mat_pvez_pre_Nacional)  


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, DT11, DT12, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_pvez_pre_Nacional %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados Universidad

MtPvezPre1100 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPvezPre1100")


# Mat pvz pre Bogotá 1101 ---- 


# Modificar niveles y crear factor para edad 


Mat_pvez_pre_Bogota <- Mat_pvez_pre_Bogota %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_pvez_pre_Bogota$CAT_EDAD <- factor(Mat_pvez_pre_Bogota$CAT_EDAD, levels = c('17 años o menos', '18 a 20 años', '21 a 25 años', '26 o más años', 'Sin información'))
Mat_pvez_pre_Bogota$PAES <- factor(Mat_pvez_pre_Bogota$PAES, levels = c('Comunidades indígenas', 'Mejores bachilleres', 'Mejores bachilleres de municipios pobres', 'Población afrocolombiana', 'Victimas del conflicto armado interno en Colombia'))


# Tabla agregada

DT1 <- Agregar(Mat_pvez_pre_Bogota, 'FACULTAD')
DT2 <- Agregar(Mat_pvez_pre_Bogota, 'NACIONALIDAD')
DT2 <- mutate(DT2, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT3 <- Agregar(Mat_pvez_pre_Bogota, 'SEXO')
DT4 <- Agregar(Mat_pvez_pre_Bogota, 'CAT_EDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT5 <- Agregar(Mat_pvez_pre_Bogota, 'ESTRATO')
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT6 <- Agregar(Mat_pvez_pre_Bogota, 'TIPO_COL')
DT6 <- DT6 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT7 <- Agregar(Mat_pvez_pre_Bogota, 'PBM')
DT7 <- DT7 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT8 <- Agregar(Mat_pvez_pre_Bogota, 'MOD_ADM')
DT9 <- Agregar(Mat_pvez_pre_Bogota, 'TIPO_ADM')
DT10 <- Agregar(Mat_pvez_pre_Bogota, 'PAES')
DT10 <- DT10 %>% filter(!is.na(Clase))
DT11 <- Agregar(Mat_pvez_pre_Bogota, 'PEAMA')
DT11 <- DT11 %>% filter(!is.na(Clase))
DT12 <- Agregar(Mat_pvez_pre_Bogota, 'AREAC_SNIES')
Total <- Totales(Mat_pvez_pre_Bogota)  


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, DT11, DT12, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_pvez_pre_Bogota %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados Universidad

MtPvezPre1101 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPvezPre1101")


# Mat pvz pre Medellín 1102 ---- 


# Modificar niveles y crear factor para edad 


Mat_pvez_pre_Medellin <- Mat_pvez_pre_Medellin %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_pvez_pre_Medellin$CAT_EDAD <- factor(Mat_pvez_pre_Medellin$CAT_EDAD, levels = c('17 años o menos', '18 a 20 años', '21 a 25 años', '26 o más años', 'Sin información'))
Mat_pvez_pre_Medellin$PAES <- factor(Mat_pvez_pre_Medellin$PAES, levels = c('Comunidades indígenas', 'Mejores bachilleres', 'Mejores bachilleres de municipios pobres', 'Población afrocolombiana', 'Victimas del conflicto armado interno en Colombia'))


# Tabla agregada

DT1 <- Agregar(Mat_pvez_pre_Medellin, 'FACULTAD')
DT2 <- Agregar(Mat_pvez_pre_Medellin, 'NACIONALIDAD')
DT2 <- mutate(DT2, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT3 <- Agregar(Mat_pvez_pre_Medellin, 'SEXO')
DT4 <- Agregar(Mat_pvez_pre_Medellin, 'CAT_EDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT5 <- Agregar(Mat_pvez_pre_Medellin, 'ESTRATO')
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT6 <- Agregar(Mat_pvez_pre_Medellin, 'TIPO_COL')
DT6 <- DT6 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT7 <- Agregar(Mat_pvez_pre_Medellin, 'PBM')
DT7 <- DT7 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT8 <- Agregar(Mat_pvez_pre_Medellin, 'MOD_ADM')
DT9 <- Agregar(Mat_pvez_pre_Medellin, 'TIPO_ADM')
DT10 <- Agregar(Mat_pvez_pre_Medellin, 'PAES')
DT10 <- DT10 %>% filter(!is.na(Clase))
DT11 <- Agregar(Mat_pvez_pre_Medellin, 'PEAMA')
DT11 <- DT11 %>% filter(!is.na(Clase))
DT12 <- Agregar(Mat_pvez_pre_Medellin, 'AREAC_SNIES')
Total <- Totales(Mat_pvez_pre_Medellin)  


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, DT11, DT12, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_pvez_pre_Medellin %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados Universidad

MtPvezPre1102 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPvezPre1102")


# Mat pvz pre Manizales 1103 ---- 


# Modificar niveles y crear factor para edad 


Mat_pvez_pre_Manizales <- Mat_pvez_pre_Manizales %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_pvez_pre_Manizales$CAT_EDAD <- factor(Mat_pvez_pre_Manizales$CAT_EDAD, levels = c('17 años o menos', '18 a 20 años', '21 a 25 años', '26 o más años', 'Sin información'))
Mat_pvez_pre_Manizales$PAES <- factor(Mat_pvez_pre_Manizales$PAES, levels = c('Comunidades indígenas', 'Mejores bachilleres', 'Mejores bachilleres de municipios pobres', 'Población afrocolombiana', 'Victimas del conflicto armado interno en Colombia'))


# Tabla agregada

DT1 <- Agregar(Mat_pvez_pre_Manizales, 'FACULTAD')
DT2 <- Agregar(Mat_pvez_pre_Manizales, 'NACIONALIDAD')
DT2 <- mutate(DT2, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT3 <- Agregar(Mat_pvez_pre_Manizales, 'SEXO')
DT4 <- Agregar(Mat_pvez_pre_Manizales, 'CAT_EDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT5 <- Agregar(Mat_pvez_pre_Manizales, 'ESTRATO')
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT6 <- Agregar(Mat_pvez_pre_Manizales, 'TIPO_COL')
DT6 <- DT6 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT7 <- Agregar(Mat_pvez_pre_Manizales, 'PBM')
DT7 <- DT7 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT8 <- Agregar(Mat_pvez_pre_Manizales, 'MOD_ADM')
DT9 <- Agregar(Mat_pvez_pre_Manizales, 'TIPO_ADM')
DT10 <- Agregar(Mat_pvez_pre_Manizales, 'PAES')
DT10 <- DT10 %>% filter(!is.na(Clase))
DT11 <- Agregar(Mat_pvez_pre_Manizales, 'PEAMA')
DT11 <- DT11 %>% filter(!is.na(Clase))
DT12 <- Agregar(Mat_pvez_pre_Manizales, 'AREAC_SNIES')
Total <- Totales(Mat_pvez_pre_Manizales)  


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, DT11, DT12, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_pvez_pre_Manizales %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados Universidad

MtPvezPre1103 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPvezPre1103")


# Mat pvz pre Palmira 1104 ---- 


# Modificar niveles y crear factor para edad 


Mat_pvez_pre_Palmira <- Mat_pvez_pre_Palmira %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_pvez_pre_Palmira$CAT_EDAD <- factor(Mat_pvez_pre_Palmira$CAT_EDAD, levels = c('17 años o menos', '18 a 20 años', '21 a 25 años', '26 o más años', 'Sin información'))
Mat_pvez_pre_Palmira$PAES <- factor(Mat_pvez_pre_Palmira$PAES, levels = c('Comunidades indígenas', 'Mejores bachilleres', 'Mejores bachilleres de municipios pobres', 'Población afrocolombiana', 'Victimas del conflicto armado interno en Colombia'))


# Tabla agregada

DT1 <- Agregar(Mat_pvez_pre_Palmira, 'FACULTAD')
DT2 <- Agregar(Mat_pvez_pre_Palmira, 'NACIONALIDAD')
DT2 <- mutate(DT2, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT3 <- Agregar(Mat_pvez_pre_Palmira, 'SEXO')
DT4 <- Agregar(Mat_pvez_pre_Palmira, 'CAT_EDAD')
DT4 <- mutate(DT4, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT5 <- Agregar(Mat_pvez_pre_Palmira, 'ESTRATO')
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT6 <- Agregar(Mat_pvez_pre_Palmira, 'TIPO_COL')
DT6 <- DT6 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT7 <- Agregar(Mat_pvez_pre_Palmira, 'PBM')
DT7 <- DT7 %>% filter(YEAR != 2009) %>% mutate(Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT8 <- Agregar(Mat_pvez_pre_Palmira, 'MOD_ADM')
DT9 <- Agregar(Mat_pvez_pre_Palmira, 'TIPO_ADM')
DT10 <- Agregar(Mat_pvez_pre_Palmira, 'PAES')
DT10 <- DT10 %>% filter(!is.na(Clase))
DT11 <- Agregar(Mat_pvez_pre_Palmira, 'PEAMA')
DT11 <- DT11 %>% filter(!is.na(Clase))
DT12 <- Agregar(Mat_pvez_pre_Palmira, 'AREAC_SNIES')
Total <- Totales(Mat_pvez_pre_Palmira)  


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, DT11, DT12, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_pvez_pre_Palmira %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados Universidad

MtPvezPre1104 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPvezPre1104")


# MATRICULA PVZ POS 1100 ---- 


Mat_pvez_pos_Nacional <- Mat_pvez_pos_Nacional %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_pvez_pos_Nacional$CAT_EDAD <- factor(Mat_pvez_pos_Nacional$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))


# Tabla agregada

DT1 <- Agregar(Mat_pvez_pos_Nacional, "NIVEL")
DT1 <- filter(DT1, !Clase %in% c('Pregrado', 'Tecnología'))
DT2 <- Agregar(Mat_pvez_pos_Nacional, 'SEDE_NOMBRE_MAT')
DT2 <- DT2 %>% filter(!(Clase%in%c('La Paz', 'Orinoquía', 'Tumaco')))
DT3 <- Agregar(Mat_pvez_pos_Nacional, "NACIONALIDAD")
DT3 <- mutate(DT3, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT4 <- Agregar(Mat_pvez_pos_Nacional, "SEXO")
DT5 <- Agregar(Mat_pvez_pos_Nacional, "CAT_EDAD")
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT6 <- Agregar(Mat_pvez_pos_Nacional, "CONVENIO")
DT6 <- DT6 %>% filter(!YEAR %in% c(2009, 2010), !(YEAR == 2011 & SEMESTRE == 1))
DT7 <- Agregar(Mat_pvez_pos_Nacional, "TIP_CONVENIO")
DT7 <- DT7 %>% filter(!YEAR %in% c(2009, 2010, 2011), Clase != "Sin información") 
DT8 <- Agregar(Mat_pvez_pos_Nacional, "AREAC_SNIES")
Total <- Totales(Mat_pvez_pos_Nacional)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_pvez_pos_Nacional %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

MtPvezPos1100 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPvezPos1100")


# Mat pvz pos Bogotá 1101 ---- 


Mat_pvez_pos_Bogota <- Mat_pvez_pos_Bogota %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_pvez_pos_Bogota$CAT_EDAD <- factor(Mat_pvez_pos_Bogota$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))


# Tabla agregada

DT1 <- Agregar(Mat_pvez_pos_Bogota, "NIVEL")
DT1 <- filter(DT1, !Clase %in% c('Pregrado', 'Tecnología'))
DT2 <- Agregar(Mat_pvez_pos_Bogota, 'FACULTAD')
DT3 <- Agregar(Mat_pvez_pos_Bogota, "NACIONALIDAD")
DT3 <- mutate(DT3, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT4 <- Agregar(Mat_pvez_pos_Bogota, "SEXO")
DT5 <- Agregar(Mat_pvez_pos_Bogota, "CAT_EDAD")
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT6 <- Agregar(Mat_pvez_pos_Bogota, "CONVENIO")
DT6 <- DT6 %>% filter(!YEAR %in% c(2009, 2010), !(YEAR == 2011 & SEMESTRE == 1))
DT7 <- Agregar(Mat_pvez_pos_Bogota, "TIP_CONVENIO")
DT7 <- DT7 %>% filter(!YEAR %in% c(2009, 2010, 2011), Clase != "Sin información") 
DT8 <- Agregar(Mat_pvez_pos_Bogota, "AREAC_SNIES")
Total <- Totales(Mat_pvez_pos_Bogota)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_pvez_pos_Bogota %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

MtPvezPos1101 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPvezPos1101")



# Mat pvz pos Medellín 1102 ---- 


Mat_pvez_pos_Medellin <- Mat_pvez_pos_Medellin %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_pvez_pos_Medellin$CAT_EDAD <- factor(Mat_pvez_pos_Medellin$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))


# Tabla agregada

DT1 <- Agregar(Mat_pvez_pos_Medellin, "NIVEL")
DT1 <- filter(DT1, !Clase %in% c('Pregrado', 'Tecnología'))
DT2 <- Agregar(Mat_pvez_pos_Medellin, 'FACULTAD')
DT3 <- Agregar(Mat_pvez_pos_Medellin, "NACIONALIDAD")
DT3 <- mutate(DT3, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT4 <- Agregar(Mat_pvez_pos_Medellin, "SEXO")
DT5 <- Agregar(Mat_pvez_pos_Medellin, "CAT_EDAD")
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT6 <- Agregar(Mat_pvez_pos_Medellin, "CONVENIO")
DT6 <- DT6 %>% filter(!YEAR %in% c(2009, 2010), !(YEAR == 2011 & SEMESTRE == 1))
DT7 <- Agregar(Mat_pvez_pos_Medellin, "TIP_CONVENIO")
DT7 <- DT7 %>% filter(!YEAR %in% c(2009, 2010, 2011), Clase != "Sin información") 
DT8 <- Agregar(Mat_pvez_pos_Medellin, "AREAC_SNIES")
Total <- Totales(Mat_pvez_pos_Medellin)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_pvez_pos_Medellin %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

MtPvezPos1102 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPvezPos1102")


# Mat pvz pos Manizales 1103 ---- 


Mat_pvez_pos_Manizales <- Mat_pvez_pos_Manizales %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_pvez_pos_Manizales$CAT_EDAD <- factor(Mat_pvez_pos_Manizales$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))


# Tabla agregada

DT1 <- Agregar(Mat_pvez_pos_Manizales, "NIVEL")
DT1 <- filter(DT1, !Clase %in% c('Pregrado', 'Tecnología'))
DT2 <- Agregar(Mat_pvez_pos_Manizales, 'FACULTAD')
DT3 <- Agregar(Mat_pvez_pos_Manizales, "NACIONALIDAD")
DT3 <- mutate(DT3, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT4 <- Agregar(Mat_pvez_pos_Manizales, "SEXO")
DT5 <- Agregar(Mat_pvez_pos_Manizales, "CAT_EDAD")
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT6 <- Agregar(Mat_pvez_pos_Manizales, "CONVENIO")
DT6 <- DT6 %>% filter(!YEAR %in% c(2009, 2010), !(YEAR == 2011 & SEMESTRE == 1))
DT7 <- Agregar(Mat_pvez_pos_Manizales, "TIP_CONVENIO")
DT7 <- DT7 %>% filter(!YEAR %in% c(2009, 2010, 2011), Clase != "Sin información") 
DT8 <- Agregar(Mat_pvez_pos_Manizales, "AREAC_SNIES")
Total <- Totales(Mat_pvez_pos_Manizales)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_pvez_pos_Manizales %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

MtPvezPos1103 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPvezPos1103")


# Mat pvz pos Palmira 1104 ---- 


Mat_pvez_pos_Palmira <- Mat_pvez_pos_Palmira %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Mat_pvez_pos_Palmira$CAT_EDAD <- factor(Mat_pvez_pos_Palmira$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))


# Tabla agregada

DT1 <- Agregar(Mat_pvez_pos_Palmira, "NIVEL")
DT1 <- filter(DT1, !Clase %in% c('Pregrado', 'Tecnología'))
DT2 <- Agregar(Mat_pvez_pos_Palmira, 'FACULTAD')
DT3 <- Agregar(Mat_pvez_pos_Palmira, "NACIONALIDAD")
DT3 <- mutate(DT3, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT4 <- Agregar(Mat_pvez_pos_Palmira, "SEXO")
DT5 <- Agregar(Mat_pvez_pos_Palmira, "CAT_EDAD")
DT5 <- mutate(DT5, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total)) 
DT6 <- Agregar(Mat_pvez_pos_Palmira, "CONVENIO")
DT6 <- DT6 %>% filter(!YEAR %in% c(2009, 2010), !(YEAR == 2011 & SEMESTRE == 1))
DT7 <- Agregar(Mat_pvez_pos_Palmira, "TIP_CONVENIO")
DT7 <- DT7 %>% filter(!YEAR %in% c(2009, 2010, 2011), Clase != "Sin información") 
DT8 <- Agregar(Mat_pvez_pos_Palmira, "AREAC_SNIES")
Total <- Totales(Mat_pvez_pos_Palmira)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Mat_pvez_pos_Palmira %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = parse_number(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

MtPvezPos1104 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = parse_number(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "MtPvezPos1104")


# Consolidado Estadísticas ----

ConsolidadoG <- bind_rows(Mt1100, Mt1101, Mt1102, Mt1103, Mt1104,
                          MtPre1100, MtPre1101, MtPre1102, MtPre1103, MtPre1104,
                          MtPos1100, MtPos1101, MtPos1102, MtPos1103, MtPos1104,
                          MtPvez1100, MtPvez1101, MtPvez1102, MtPvez1103, MtPvez1104,
                          MtPvezPre1100, MtPvezPre1101, MtPvezPre1102, MtPvezPre1103, MtPvezPre1104,
                          MtPvezPos1100, MtPvezPos1101, MtPvezPos1102, MtPvezPos1103, MtPvezPos1104)

# Etiquetas último periodo ----

# Año
ano <- ConsolidadoG %>% unite("YEAR", YEAR, sep = "")%>%
  mutate(YEAR = parse_number(YEAR))
ano <- max(ano %>% select(YEAR))

#Semestre
semestre <- ConsolidadoG %>% unite("SEMESTRE", SEMESTRE, sep = "")%>%
  mutate(SEMESTRE = parse_number(SEMESTRE))

semestre <- semestre[[nrow(semestre), "SEMESTRE"]]

#Periodo Actual
periodo_actual_titulo <- paste0(" ", ano, "-", semestre)


ano
semestre
periodo_actual_titulo

# Remover archivos ----

rm(Agregado, DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, DT11,
   DT12, DT13, DT14, Mt1100, Mt1101, Mt1102, Mt1103, Mt1104,
   MtPre1100, MtPre1101, MtPre1102, MtPre1103, MtPre1104,
   MtPos1100, MtPos1101, MtPos1102, MtPos1103, MtPos1104,
   MtPvez1100, MtPvez1101, MtPvez1102, MtPvez1103, MtPvez1104,
   MtPvezPre1100, MtPvezPre1101, MtPvezPre1102, MtPvezPre1103, MtPvezPre1104,
   MtPvezPos1100, MtPvezPos1101, MtPvezPos1102, MtPvezPos1103, MtPvezPos1104,
   Total, Uperiodo, M2009, M2010, M2011, M2012, M2013, M2014, M2015, M2016,
   M2017,M20181,M20182, M20191, M20192, M20201, M20211, importar, tipovar)
   
   gc()

   
# 
#    Mat_Nacional, Mat_Bogota, Mat_Medellin, Mat_Manizales, Mat_Palmira,
#    Mat_Pre_Nacional, Mat_Pre_Bogota, Mat_Pre_Medellin, Mat_Pre_Manizales,  Mat_Pre_Palmira, 
#    Mat_Pos_Nacional, Mat_Pos_Bogota, Mat_Pos_Medellin, Mat_Pos_Manizales, Mat_Pos_Palmira,
#    Mat_pvez_Nacional, Mat_pvez_Bogota, Mat_pvez_Medellin, Mat_pvez_Manizales, Mat_pvez_Palmira,
#    Mat_pvez_pre_Nacional, Mat_pvez_pre_Bogota, Mat_pvez_pre_Medellin, Mat_pvez_pre_Manizales, Mat_pvez_pre_Palmira,
#    Mat_pvez_pos_Nacional, Mat_pvez_pos_Bogota, Mat_pvez_pos_Medellin, Mat_pvez_pos_Manizales, Mat_pvez_pos_Palmira,



