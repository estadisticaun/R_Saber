
library("googledrive")
library(tidyverse) # version 1.2.1
library(readxl)    # version 1.0.0


# Seleccionar Matriculados SPN -Para Boletines Estadísticos-

MatriculadosSPN <- Microdatos %>% filter(TIPO_ADM == 'PEAMA'|  
                                         SEDE_NOMBRE_ADM == 'De La Paz'| 
                                         (TIPO_NIVEL == 'Postgrado'& SEDE_NOMBRE_ADM %in% c('Amazonía', 'Caribe')))

# Importar microdatos a CSV

write.csv(MatriculadosSPN, file = "MatriculadosSPN.csv")

# Exportar a Drive de estadísticas.unal

drive_upload('MatriculadosSPN.csv', 'Agregados SPN/Matriculados/MatriculadosSPN.csv', overwrite = TRUE)

# Remover archivo de matriculados en SPN

unlink(c("MatriculadosSPN.csv"))

