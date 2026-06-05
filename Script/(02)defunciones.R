# ==============================================================================
# Script 3: Suavizado y Promedio de Defunciones para Años Referencia (Coahuila)
# ==============================================================================

# Carga de paquetes ----
library(data.table)

# Carga de datos prorrateados ----
def_pro <- fread("Data/def_pro.csv")

# ==============================================================================
# 1. ASIGNACIÓN DE AÑOS DE REFERENCIA Y PROMEDIO 
# ==============================================================================

# Agrupamos los años alrededor del censo 2010 y censo 2020 con comas correctas
def_pro[, year_new := ifelse(year %in% 2009:2011, 2010, ifelse(year %in% 2018:2019, 2019, year))]

# Calculamos la media de defunciones
def <- def_pro[, .(deaths = mean(deaths)), .(year = year_new, sex, age)]

# ==============================================================================
# 2. GUARDAR TABLA FINAL DE DEFUNCIONES 
# ==============================================================================

write.csv(def, "Data/def.csv", row.names = FALSE)
