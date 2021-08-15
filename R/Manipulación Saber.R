#################################################################################
# ANÁLISIS DE RESULTADOS DE LOS EXAMENES SABER PRO
#################################################################################

# LIBRERÍAS REQUERIDAS
library(readxl)  # version 1.0.0
library(tidyverse)
library(ggrepel)
library(plotly)
library(DT)          # version 0.4
library(highcharter) # version 0.5.0.9999


# IMPORTAR DATOS SABER PRO 2017

SBPRO_2017_GEN <- read_excel("Datos/SBPRO-2017-GEN.xlsx")

# SELECCIONAR VARIABLES DE INTERÉS

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% select(c(INST_COD_INSTITUCION:GRUPOREFERENCIA, 
                                              MOD_RAZONA_CUANTITAT_PUNT, MOD_RAZONA_CUANTITATIVO_PNAL, MOD_RAZONA_CUANTITATIVO_PGREF,
                                              MOD_LECTURA_CRITICA_PUNT, MOD_LECTURA_CRITICA_PNAL, MOD_LECTURA_CRITICA_PGREF,
                                              MOD_COMPETEN_CIUDADA_PUNT, MOD_COMPETEN_CIUDADA_PNAL, MOD_COMPETEN_CIUDADA_PGREF,
                                              MOD_INGLES_PUNT, MOD_INGLES_PNAL, MOD_INGLES_PGREF, MOD_COMUNI_ESCRITA_PUNT,
                                              MOD_COMUNI_ESCRITA_PNAL, MOD_COMUNI_ESCRITA_PGREF, PUNT_GLOBAL, PERCENTIL_GLOBAL))

# CREAR VARIABLES DE INTERÉS

# Crear Variable Unal

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(Unal =  case_when(.$INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104, 1124, 1125, 1126, 9920) ~ "UN",
                           TRUE ~ "Resto IES"))
# Crear Sedes Unal

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(Sedes =  case_when(.$INST_COD_INSTITUCION == 1101 ~ "UN-Bogota",
                            .$INST_COD_INSTITUCION == 1102 ~ "UN-Medellin",
                            .$INST_COD_INSTITUCION == 1103 ~ "UN-Manizales",
                            .$INST_COD_INSTITUCION == 1104 ~ "UN-Palmira",
                            .$INST_COD_INSTITUCION == 1124 ~ "UN-Orinoquia",
                            .$INST_COD_INSTITUCION == 1125 ~ "UN-Amazonia",
                            .$INST_COD_INSTITUCION == 1126 ~ "UN-Caribe",
                            .$INST_COD_INSTITUCION == 9920 ~ "UN-Tumaco",
                            TRUE ~ "Resto IES"))

# Crear G12

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(G12 =  case_when(.$INST_COD_INSTITUCION == 1813  ~ "U. de los Andes",
                          .$INST_COD_INSTITUCION == 1714  ~ "U. del Rosario",
                          .$INST_COD_INSTITUCION == 1706  ~ "U. Externado",
                          .$INST_COD_INSTITUCION == 1828  ~ "ICESI",
                          .$INST_COD_INSTITUCION %in% c(1201, 1219, 1222, 1223, 9125) ~ "U. de Antioquia",
                          .$INST_COD_INSTITUCION == 1712  ~ "EAFIT",
                          .$INST_COD_INSTITUCION == 1203  ~ "U. del Valle",
                          .$INST_COD_INSTITUCION %in% c(1701, 1702)  ~ "U. Javeriana",
                          .$INST_COD_INSTITUCION == 1204  ~ "UIS",
                          .$INST_COD_INSTITUCION == 1713  ~ "U. del Norte",
                          .$INST_COD_INSTITUCION %in% c(1710, 1723, 1727, 1730)  ~ "U. Bolivariana",
                          .$INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104)  ~ "U. Nacional",
                          TRUE ~ "Resto IES"))

# Crear G16 (sedes Unal)
SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(G15 =  case_when(.$INST_COD_INSTITUCION == 1813  ~ "U. de los Andes",
                          .$INST_COD_INSTITUCION == 1714  ~ "U. del Rosario",
                          .$INST_COD_INSTITUCION == 1706  ~ "U. Externado",
                          .$INST_COD_INSTITUCION == 1828  ~ "ICESI",
                          .$INST_COD_INSTITUCION %in% c(1201, 1219, 1222, 1223, 9125) ~ "U. de Antioquia",
                          .$INST_COD_INSTITUCION == 1712  ~ "EAFIT",
                          .$INST_COD_INSTITUCION == 1203  ~ "U. del Valle",
                          .$INST_COD_INSTITUCION %in% c(1701, 1702)  ~ "U. Javeriana",
                          .$INST_COD_INSTITUCION == 1204  ~ "UIS",
                          .$INST_COD_INSTITUCION == 1713  ~ "U. del Norte",
                          .$INST_COD_INSTITUCION %in% c(1710, 1723, 1727, 1730)  ~ "U. Bolivariana",
                          .$INST_COD_INSTITUCION == 1101  ~ "UN-Bogota",
                          .$INST_COD_INSTITUCION == 1102  ~ "UN-Medellin",
                          .$INST_COD_INSTITUCION == 1103  ~ "UN-Manizales",
                          .$INST_COD_INSTITUCION == 1104  ~ "UN-Palmira",
                          TRUE ~ "Resto IES"))

# Remover datos faltantes de grupos de referencia

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% filter(!is.na(GRUPOREFERENCIA))


# Distribución puntajes globales, unal y sedes

### crear base de datos 

Unal <- SBPRO_2017_GEN %>% select(PUNT_GLOBAL, Unal) %>% rename(Categoria = Unal)
sedes <- SBPRO_2017_GEN %>% select(PUNT_GLOBAL, Sedes) %>% 
  filter(Sedes != "Resto IES") %>% rename(Categoria = Sedes)
Global <- bind_rows(Unal, sedes)

####################################################################
################# ANÁLISIS POR FACULTADES 
####################################################################

# RAZONAMIENTO CUANTITATIVO

QCuantitativo <- SBPRO_2017_GEN %>% 
  mutate(QRazCuant =  case_when(.$MOD_RAZONA_CUANTITATIVO_PNAL <= 20  ~ "Quintil1",
                                .$MOD_RAZONA_CUANTITATIVO_PNAL > 20 & .$MOD_RAZONA_CUANTITATIVO_PNAL <= 40 ~ "Quintil2",
                                .$MOD_RAZONA_CUANTITATIVO_PNAL > 40 & .$MOD_RAZONA_CUANTITATIVO_PNAL <= 60 ~ "Quintil3",
                                .$MOD_RAZONA_CUANTITATIVO_PNAL > 60 & .$MOD_RAZONA_CUANTITATIVO_PNAL <= 80 ~ "Quintil4",
                                TRUE ~ "Quintil5"
  )) %>% 
  select(G12,G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  filter(G12 == "U. Nacional") %>% 
  group_by(G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  summarise(Total = n()) %>% spread(QRazCuant, Total) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0)) %>% 
  ungroup() %>% 
  mutate(G15 = str_replace(G15, "UN-", ""))

# Insertar Facultades

Facultades <- read_excel("Datos/Facultades.xlsx")

QCuantitativo <- left_join(QCuantitativo, Facultades, by = "ESTU_SNIES_PRGMACADEMICO")


RC_Facul <- QCuantitativo %>%  group_by(SEDE, FACULTAD) %>% 
  summarise(Quintil1 = sum(Quintil1), Quintil2 = sum(Quintil2),
            Quintil3 = sum(Quintil3), Quintil4 = sum(Quintil4),
            Quintil5 = sum(Quintil5)) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0))

# COMPETENCIAS CIUDADANAS

Qciudadanas <- SBPRO_2017_GEN %>% 
  mutate(QRazCuant =  case_when(.$MOD_COMPETEN_CIUDADA_PNAL <= 20  ~ "Quintil1",
                                .$MOD_COMPETEN_CIUDADA_PNAL > 20 & .$MOD_COMPETEN_CIUDADA_PNAL <= 40 ~ "Quintil2",
                                .$MOD_COMPETEN_CIUDADA_PNAL > 40 & .$MOD_COMPETEN_CIUDADA_PNAL <= 60 ~ "Quintil3",
                                .$MOD_COMPETEN_CIUDADA_PNAL > 60 & .$MOD_COMPETEN_CIUDADA_PNAL <= 80 ~ "Quintil4",
                                TRUE ~ "Quintil5"
  )) %>% 
  select(G12,G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  filter(G12 == "U. Nacional") %>% 
  group_by(G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  summarise(Total = n()) %>% spread(QRazCuant, Total) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0)) %>% 
  ungroup() %>% 
  mutate(G15 = str_replace(G15, "UN-", ""))


# Insertar Facultades

Facultades <- read_excel("Datos/Facultades.xlsx")

Qciudadanas <- left_join(Qciudadanas, Facultades, by = "ESTU_SNIES_PRGMACADEMICO")


CC_Facul <- Qciudadanas %>%  group_by(SEDE, FACULTAD) %>% 
  summarise(Quintil1 = sum(Quintil1), Quintil2 = sum(Quintil2),
            Quintil3 = sum(Quintil3), Quintil4 = sum(Quintil4),
            Quintil5 = sum(Quintil5)) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0))


# LECTURA CRÍTICA


Lcritica <- SBPRO_2017_GEN %>% 
  mutate(QRazCuant =  case_when(.$MOD_LECTURA_CRITICA_PNAL <= 20  ~ "Quintil1",
                                .$MOD_LECTURA_CRITICA_PNAL > 20 & .$MOD_LECTURA_CRITICA_PNAL <= 40 ~ "Quintil2",
                                .$MOD_LECTURA_CRITICA_PNAL > 40 & .$MOD_LECTURA_CRITICA_PNAL <= 60 ~ "Quintil3",
                                .$MOD_LECTURA_CRITICA_PNAL > 60 & .$MOD_LECTURA_CRITICA_PNAL <= 80 ~ "Quintil4",
                                TRUE ~ "Quintil5"
  )) %>% 
  select(G12,G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  filter(G12 == "U. Nacional") %>% 
  group_by(G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  summarise(Total = n()) %>% spread(QRazCuant, Total) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0)) %>% 
  ungroup() %>% 
  mutate(G15 = str_replace(G15, "UN-", ""))


# Insertar Facultades

Facultades <- read_excel("Datos/Facultades.xlsx")

Lcritica <- left_join(Lcritica, Facultades, by = "ESTU_SNIES_PRGMACADEMICO")


LC_Facul <- Lcritica %>%  group_by(SEDE, FACULTAD) %>% 
  summarise(Quintil1 = sum(Quintil1), Quintil2 = sum(Quintil2),
            Quintil3 = sum(Quintil3), Quintil4 = sum(Quintil4),
            Quintil5 = sum(Quintil5)) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0))


# COMUNICACIÓN ESCRITA

Cescrita <- SBPRO_2017_GEN %>% 
  mutate(QRazCuant =  case_when(.$MOD_COMUNI_ESCRITA_PNAL <= 20  ~ "Quintil1",
                                .$MOD_COMUNI_ESCRITA_PNAL > 20 & .$MOD_COMUNI_ESCRITA_PNAL <= 40 ~ "Quintil2",
                                .$MOD_COMUNI_ESCRITA_PNAL > 40 & .$MOD_COMUNI_ESCRITA_PNAL <= 60 ~ "Quintil3",
                                .$MOD_COMUNI_ESCRITA_PNAL > 60 & .$MOD_COMUNI_ESCRITA_PNAL <= 80 ~ "Quintil4",
                                TRUE ~ "Quintil5"
  )) %>% 
  select(G12,G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  filter(G12 == "U. Nacional") %>% 
  group_by(G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  summarise(Total = n()) %>% spread(QRazCuant, Total) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0)) %>% 
  ungroup() %>% 
  mutate(G15 = str_replace(G15, "UN-", ""))



# Insertar Facultades

Facultades <- read_excel("Datos/Facultades.xlsx")

Cescrita <- left_join(Cescrita, Facultades, by = "ESTU_SNIES_PRGMACADEMICO")


CE_Facul <- Cescrita %>%  group_by(SEDE, FACULTAD) %>% 
  summarise(Quintil1 = sum(Quintil1), Quintil2 = sum(Quintil2),
            Quintil3 = sum(Quintil3), Quintil4 = sum(Quintil4),
            Quintil5 = sum(Quintil5)) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0))

# INGLÉS


qingles <- SBPRO_2017_GEN %>% 
  mutate(QRazCuant =  case_when(.$MOD_INGLES_PNAL <= 20  ~ "Quintil1",
                                .$MOD_INGLES_PNAL > 20 & .$MOD_INGLES_PNAL <= 40 ~ "Quintil2",
                                .$MOD_INGLES_PNAL > 40 & .$MOD_INGLES_PNAL <= 60 ~ "Quintil3",
                                .$MOD_INGLES_PNAL > 60 & .$MOD_INGLES_PNAL <= 80 ~ "Quintil4",
                                TRUE ~ "Quintil5"
  )) %>% 
  select(G12,G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  filter(G12 == "U. Nacional") %>% 
  group_by(G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  summarise(Total = n()) %>% spread(QRazCuant, Total) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0)) %>% 
  ungroup() %>% 
  mutate(G15 = str_replace(G15, "UN-", ""))


# Insertar Facultades

Facultades <- read_excel("Datos/Facultades.xlsx")

qingles <- left_join(qingles, Facultades, by = "ESTU_SNIES_PRGMACADEMICO")


IN_Facul <- qingles %>%  group_by(SEDE, FACULTAD) %>% 
  summarise(Quintil1 = sum(Quintil1), Quintil2 = sum(Quintil2),
            Quintil3 = sum(Quintil3), Quintil4 = sum(Quintil4),
            Quintil5 = sum(Quintil5)) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0))


####################################################################
################# ANÁLISIS POR PROGRAMAS ACADÉMICOS
####################################################################

# RAZONAMIENTO CUANTITATIVO

RC_Prog <- SBPRO_2017_GEN %>% 
  mutate(QRazCuant =  case_when(.$MOD_RAZONA_CUANTITATIVO_PNAL <= 20  ~ "Quintil1",
                                .$MOD_RAZONA_CUANTITATIVO_PNAL > 20 & .$MOD_RAZONA_CUANTITATIVO_PNAL <= 40 ~ "Quintil2",
                                .$MOD_RAZONA_CUANTITATIVO_PNAL > 40 & .$MOD_RAZONA_CUANTITATIVO_PNAL <= 60 ~ "Quintil3",
                                .$MOD_RAZONA_CUANTITATIVO_PNAL > 60 & .$MOD_RAZONA_CUANTITATIVO_PNAL <= 80 ~ "Quintil4",
                                TRUE ~ "Quintil5"
  )) %>% 
  select(G12,G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  filter(G12 == "U. Nacional") %>% 
  group_by(G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  summarise(Total = n()) %>% spread(QRazCuant, Total) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0)) %>% 
  ungroup() %>% 
  mutate(G15 = str_replace(G15, "UN-", "")) %>% 
  rename("Programa_Academico" = ESTU_PRGM_ACADEMICO, Sede = G15) %>% 
  mutate(Programa_Academico = str_to_title(Programa_Academico))

# COMPETENCIAS CIUDADANAS

CC_Prog <- SBPRO_2017_GEN %>% 
  mutate(QRazCuant =  case_when(.$MOD_COMPETEN_CIUDADA_PNAL <= 20  ~ "Quintil1",
                                .$MOD_COMPETEN_CIUDADA_PNAL > 20 & .$MOD_COMPETEN_CIUDADA_PNAL <= 40 ~ "Quintil2",
                                .$MOD_COMPETEN_CIUDADA_PNAL > 40 & .$MOD_COMPETEN_CIUDADA_PNAL <= 60 ~ "Quintil3",
                                .$MOD_COMPETEN_CIUDADA_PNAL > 60 & .$MOD_COMPETEN_CIUDADA_PNAL <= 80 ~ "Quintil4",
                                TRUE ~ "Quintil5"
  )) %>% 
  select(G12,G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  filter(G12 == "U. Nacional") %>% 
  group_by(G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  summarise(Total = n()) %>% spread(QRazCuant, Total) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0)) %>% 
  ungroup() %>% 
  mutate(G15 = str_replace(G15, "UN-", "")) %>% 
  rename("Programa_Academico" = ESTU_PRGM_ACADEMICO, Sede = G15) %>% 
  mutate(Programa_Academico = str_to_title(Programa_Academico))

# LECTURA CRÍTICA

LC_Prog <- SBPRO_2017_GEN %>% 
  mutate(QRazCuant =  case_when(.$MOD_LECTURA_CRITICA_PNAL <= 20  ~ "Quintil1",
                                .$MOD_LECTURA_CRITICA_PNAL > 20 & .$MOD_LECTURA_CRITICA_PNAL <= 40 ~ "Quintil2",
                                .$MOD_LECTURA_CRITICA_PNAL > 40 & .$MOD_LECTURA_CRITICA_PNAL <= 60 ~ "Quintil3",
                                .$MOD_LECTURA_CRITICA_PNAL > 60 & .$MOD_LECTURA_CRITICA_PNAL <= 80 ~ "Quintil4",
                                TRUE ~ "Quintil5"
  )) %>% 
  select(G12,G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  filter(G12 == "U. Nacional") %>% 
  group_by(G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  summarise(Total = n()) %>% spread(QRazCuant, Total) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0)) %>% 
  ungroup() %>% 
  mutate(G15 = str_replace(G15, "UN-", "")) %>% 
  rename("Programa_Academico" = ESTU_PRGM_ACADEMICO, Sede = G15) %>% 
  mutate(Programa_Academico = str_to_title(Programa_Academico))

# COMUNICACIÓN ESCRITA

CE_Prog <- SBPRO_2017_GEN %>% 
  mutate(QRazCuant =  case_when(.$MOD_COMUNI_ESCRITA_PNAL <= 20  ~ "Quintil1",
                                .$MOD_COMUNI_ESCRITA_PNAL > 20 & .$MOD_COMUNI_ESCRITA_PNAL <= 40 ~ "Quintil2",
                                .$MOD_COMUNI_ESCRITA_PNAL > 40 & .$MOD_COMUNI_ESCRITA_PNAL <= 60 ~ "Quintil3",
                                .$MOD_COMUNI_ESCRITA_PNAL > 60 & .$MOD_COMUNI_ESCRITA_PNAL <= 80 ~ "Quintil4",
                                TRUE ~ "Quintil5"
  )) %>% 
  select(G12,G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  filter(G12 == "U. Nacional") %>% 
  group_by(G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  summarise(Total = n()) %>% spread(QRazCuant, Total) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0)) %>% 
  ungroup() %>% 
  mutate(G15 = str_replace(G15, "UN-", "")) %>% 
  rename("Programa_Academico" = ESTU_PRGM_ACADEMICO, Sede = G15) %>% 
  mutate(Programa_Academico = str_to_title(Programa_Academico))

# INGLÉS

IN_Prog <- SBPRO_2017_GEN %>% 
  mutate(QRazCuant =  case_when(.$MOD_INGLES_PNAL <= 20  ~ "Quintil1",
                                .$MOD_INGLES_PNAL > 20 & .$MOD_INGLES_PNAL <= 40 ~ "Quintil2",
                                .$MOD_INGLES_PNAL > 40 & .$MOD_INGLES_PNAL <= 60 ~ "Quintil3",
                                .$MOD_INGLES_PNAL > 60 & .$MOD_INGLES_PNAL <= 80 ~ "Quintil4",
                                TRUE ~ "Quintil5"
  )) %>% 
  select(G12,G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  filter(G12 == "U. Nacional") %>% 
  group_by(G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  summarise(Total = n()) %>% spread(QRazCuant, Total) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0)) %>% 
  ungroup() %>% 
  mutate(G15 = str_replace(G15, "UN-", "")) %>% 
  rename("Programa_Academico" = ESTU_PRGM_ACADEMICO, Sede = G15) %>% 
  mutate(Programa_Academico = str_to_title(Programa_Academico))

