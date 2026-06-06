# ==============================================================================
# Script 7: Análisis de Causa Eliminada (Homicidios) - Coahuila (CORREGIDO)
# ==============================================================================

# 1. Preámbulo ----
graphics.off()
rm(list = ls())

# 2. Carga de paquetes ----
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)
library(openxlsx)

# 3. Carga de funciones del profesor ----
source("Script/Funciones.R") 

# 4. Carga de datos ----
def_homicidios <- fread("Data/def_pro_hom.csv") 
def_totales    <- fread("Data/def.csv") # <-- Cambiado a def.csv para consistencia exacta con lt_output
apv            <- fread("Data/apv.csv")
lt_output      <- fread("Data/tablas_vida_completas.csv") 

# ==============================================================================
# 5. VERIFICAR Y HOMOLOGAR ESTRUCTURA DE DATOS ----
# ==============================================================================

if ("x" %in% names(lt_output) & !"age" %in% names(lt_output)) {
  lt_output[, age := x]
  lt_output[, x := NULL]
}

# Asegurar que las columnas de sexo mantengan el formato largo uniforme para los cruces
def_homicidios[, sex := as.character(sex)]
def_totales[, sex := as.character(sex)]
apv[, sex := as.character(sex)]
lt_output[, sex := as.character(sex)]

# ==============================================================================
# 6. PROCESAMIENTO PARA CAUSA ELIMINADA ----
# ==============================================================================

def_homicidios <- def_homicidios[year %in% c(2009, 2010, 2011, 2018, 2019, 2021)]

def_homicidios[, year_new := ifelse(year %in% 2009:2011, 2010,
                                    ifelse(year %in% 2018:2019, 2019, 2021))]

# Promediar homicidios manteniendo "male"/"female"
homicidios_promedio <- def_homicidios[, .(deaths_homicidios = mean(deaths)), 
                                      by = .(year = year_new, sex, age)]

# Unir homicidios con defunciones totales
def_completa <- merge(def_totales, homicidios_promedio, 
                      by = c("year", "sex", "age"), 
                      all.x = TRUE)

def_completa[is.na(deaths_homicidios), deaths_homicidios := 0]
def_completa[, deaths_otras := deaths - deaths_homicidios]

# Evitar valores negativos por redondeos mecánicos
def_completa[deaths_otras < 0, deaths_otras := 0]

# ==============================================================================
# 7. UNIR CON POBLACIÓN Y CALCULAR TASAS ----
# ==============================================================================

lt_input_homicidios <- merge(apv, def_completa, by = c("year", "sex", "age"), all.x = TRUE)

lt_input_homicidios[, mx_sin_homicidios := deaths_otras / N]
lt_input_homicidios[is.na(mx_sin_homicidios) | is.infinite(mx_sin_homicidios), mx_sin_homicidios := 0]
lt_input_homicidios[mx_sin_homicidios < 0, mx_sin_homicidios := 0]

# ==============================================================================
# 8. CONSTRUCCIÓN DE TABLAS DE VIDA SIN HOMICIDIOS ----
# ==============================================================================
lt_sin_homicidios <- data.table()

for (s_full in c('male', 'female')) {
  for (y in c(2010, 2019, 2021)) {
    
    temp_dt <- lt_input_homicidios[sex == s_full & year == y]
    if(nrow(temp_dt) == 0) next
    
    # Mapeo al parámetro corto ('m' o 'f') requerido por la función de tu profesor
    s_param <- ifelse(s_full == "male", "m", "f")
    
    temp_layout <- lt_abr(x = temp_dt$age, mx = temp_dt$mx_sin_homicidios, sex = s_param)
    temp_lt <- as.data.table(temp_layout)
    temp_lt[, `:=`(year = y, sex = s_full, causa = "sin_homicidios")]
    
    if ("x" %in% names(temp_lt)) {
      temp_lt[, age := x]
      temp_lt[, x := NULL]
    }
    lt_sin_homicidios <- rbind(lt_sin_homicidios, temp_lt, fill = TRUE)
  }
}

# ==============================================================================
# 9. COMBINAR TABLAS Y EXTRAER GANANCIAS EN e0 ----
# ==============================================================================
lt_output[, causa := "todas_causas"]

columnas_comunes <- intersect(names(lt_output), names(lt_sin_homicidios))
columnas_comunes <- columnas_comunes[columnas_comunes %in% 
                                       c("year", "sex", "age", "mx", "qx", "lx", "dx", "Lx", "Tx", "ex", "causa")]

lt_comparacion <- rbind(lt_output[, ..columnas_comunes], lt_sin_homicidios[, ..columnas_comunes], fill = TRUE)

# Forzar formato numérico y ordenar perfectamente por edad antes de extraer la primera fila
lt_comparacion[, age := as.numeric(gsub("-.*", "", as.character(age)))]
setorder(lt_comparacion, year, sex, causa, age)

# Extraemos la primera fila de cada grupo (edad 0) para asegurar la e0 real
e0_comparacion <- lt_comparacion[, .SD[1], by = .(year, sex, causa)][, .(year, sex, ex, causa)]

e0_todas <- e0_comparacion[causa == "todas_causas", .(year, sex, ex_todas = ex)]
e0_sin   <- e0_comparacion[causa == "sin_homicidios", .(year, sex, ex_sin = ex)]

e0_dif <- merge(e0_todas, e0_sin, by = c("year", "sex"))
e0_dif[, dif_homicidios := ex_sin - ex_todas] 

# Bautizamos etiquetas para las facetas de las gráficas
e0_comparacion[, sex_label := ifelse(sex == "male", "m", "f")]
e0_dif[, sex_label := ifelse(sex == "male", "m", "f")]
lt_comparacion[, sex_label := ifelse(sex == "male", "m", "f")]

# ==============================================================================
# 10. GENERACIÓN DE GRÁFICAS COMPARATIVAS ----
# ==============================================================================
if (!dir.exists("images/homicidios")) dir.create("images/homicidios", recursive = TRUE)

## Gráfica 1: Esperanza de vida comparativa ----
p1_horizontal <- ggplot(e0_comparacion, aes(x = causa, y = ex, fill = causa)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", ex)), vjust = -0.5, size = 4, fontface = "bold") +
  facet_grid(sex_label ~ year, 
             labeller = labeller(sex_label = c("m" = "Hombres", "f" = "Mujeres"),
                                 year = as_labeller(function(x) paste("Año", x)))) +
  scale_fill_manual(values = c("todas_causas" = "#1f77b4", "sin_homicidios" = "#fa9fb5")) +
  scale_y_continuous(limits = c(0, max(e0_comparacion$ex) * 1.15), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "ESPERANZA DE VIDA: IMPACTO DE HOMICIDIOS",
       subtitle = "Coahuila - Análisis por año y sexo", x = NULL, y = "Esperanza de Vida (años)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12),
        legend.position = "none", strip.text = element_text(face = "bold", size = 11))
ggsave("images/homicidios/e0_con_sin_homicidios.png", width = 10, height = 6, dpi = 300)

## Gráfica 2: Probabilidades de muerte (qx) ----
qx_2021 <- lt_comparacion[year == 2021 & age <= 85]

p2 <- ggplot(qx_2021, aes(x = age, y = qx, color = causa, linetype = sex_label)) +
  geom_line(linewidth = 1) +
  scale_y_log10() +
  scale_color_manual(values = c("todas_causas" = "#1f77b4", "sin_homicidios" = "#fa9fb5"),
                     labels = c("Todas las causas", "Sin homicidios")) +
  scale_linetype_manual(values = c("m" = "solid", "f" = "dashed"), labels = c("Hombres", "Mujeres")) +
  labs(title = "PROBABILIDADES DE MUERTE: IMPACTO DE HOMICIDIOS",
       subtitle = "Coahuila 2021 - Escala logarítmica", x = "Edad", y = "qx (escala logarítmica)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12), legend.position = "top")
ggsave("images/homicidios/qx_con_sin_homicidios_2021.png", width = 12, height = 6, dpi = 300)

## Gráfica 3: Ganancia en esperanza de vida ----
p3 <- ggplot(e0_dif, aes(x = factor(year), y = dif_homicidios, fill = sex_label)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.9) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", dif_homicidios), y = dif_homicidios + 0.01),
            position = position_dodge(0.8), size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("m" = "#1f77b4", "f" = "#fa9fb5"), labels = c("Hombres", "Mujeres")) +
  labs(title = "Años Ganados en Esperanza de Vida al Eliminar Homicidios",
       subtitle = "Coahuila 2010, 2019, 2021", x = NULL, y = "Diferencia de Esperanza de Vida (años)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12), legend.position = "top")
ggsave("images/homicidios/perdida_e0_homicidios.png", width = 10, height = 6, dpi = 300)

# ==============================================================================
# 11. EXPORTAR RESULTADOS ----
# ==============================================================================
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

wb <- createWorkbook()
for (sexo_full in c("male", "female")) {
  for (anio in c(2010, 2019, 2021)) {
    hoja_nombre <- paste0(ifelse(sexo_full == "male", "Hombres_", "Mujeres_"), anio)
    datos_hoja <- lt_sin_homicidios[sex == sexo_full & year == anio]
    columnas_finales <- c("age", "n", "mx", "ax", "qx", "lx", "dx", "Lx", "Tx", "ex")
    datos_hoja <- datos_hoja[, ..columnas_finales[columnas_finales %in% names(datos_hoja)]]
    addWorksheet(wb, hoja_nombre)
    writeData(wb, hoja_nombre, datos_hoja)
  }
}
addWorksheet(wb, "Resumen_e0")
writeData(wb, "Resumen_e0", e0_dif)
saveWorkbook(wb, "output/tablas_vida_sin_homicidios.xlsx", overwrite = TRUE)

fwrite(lt_comparacion, "output/comparacion_causa_eliminada.csv")
fwrite(e0_dif, "output/resumen_e0_homicidios.csv")

# ==============================================================================
# 12. RESUMEN EJECUTIVO EN CONSOLA ----
# ==============================================================================

print(e0_dif[, .(Año = year, Sexo = ifelse(sex == "male", "Hombres", "Mujeres"), E0_Todas = round(ex_todas, 2), E0_SinHom = round(ex_sin, 2), Ganancia_Años = round(dif_homicidios, 3))])
