# ==============================================================================
# Script 5: Construcción de Tablas de Vida y Esperanza de Vida - Coahuila
# ==============================================================================

# Carga de paquetes ----
library(data.table)

# Carga de las funciones del profesor ----
source("functions.R") 

# Carga de tasas centrales de mortalidad ----
mortalidad <- fread("Data/tasas_mortalidad.csv")

# ==============================================================================
# 1. GENERACIÓN DE TABLAS DE VIDA ----
# ==============================================================================

tablas_vida <- mortalidad[, {
  
  sex_param <- ifelse(sex == "male", "m", "f")
  tabla_res <- lt_abr(x = age, mx = mx, sex = sex_param)
  as.data.table(tabla_res)
  
}, by = .(year, sex)]

# Guardar todas las tablas de vida consolidadas ----
write.csv(tablas_vida, "Data/tablas_vida_completas.csv", row.names = FALSE)


# ==============================================================================
# 2. EXTRACCIÓN DE LA ESPERANZA DE VIDA AL NACER (e0) ----
# ==============================================================================

# Usamos .SD[1] por grupo para jalar la primera fila (edad 0) de forma 100% segura
esperanza_nacimiento <- tablas_vida[, .SD[1], by = .(year, sex)][, .(year, sex, e0 = round(ex, 2))]

print("=========================================================")
print("  ESPERANZA DE VIDA AL NACER (e0) EN COAHUILA  ")
print("=========================================================")
print(esperanza_nacimiento)
print("=========================================================")