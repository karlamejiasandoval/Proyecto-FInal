# ==============================================================================
# Script 6: Descomposición de Arriaga para la Esperanza de Vida - Coahuila
# ==============================================================================

# 1. Limpieza de gráficas y memoria ----
graphics.off()
rm(list = ls())

# 2. Carga de paquetes ----
library(data.table)
library(ggplot2)
library(openxlsx)
library(readxl)
library(reshape2)
library(lubridate)
library(dplyr)

# 3. Carga de funciones del profesor ----
source("Script/Funciones.R") 

# 4. Carga de tablas de vida de Coahuila ----
lt_output <- fread("Data/tablas_vida_completas.csv") 

# ==============================================================================
# 5. APLICAR DESCOMPOSICIÓN DE ARRIAGA CON INYECCIÓN DE EDAD ----
# ==============================================================================
descomposicion_resultados <- list()

# --- Período 2010-2019 ---
for (sexo in c("m", "f")) {
  sexo_full <- ifelse(sexo == "m", "male", "female")
  
  lt_2010 <- lt_output[year == 2010 & sex == sexo_full]
  lt_2019 <- lt_output[year == 2019 & sex == sexo_full]
  
  descomp <- descomposicion_arriaga(lt_2010, lt_2019)
  
  # Asignamos las variables y rescatamos la edad real desde la tabla de vida (column 'x')
  descomp$tabla_contribuciones[, `:=`(
    edad = lt_2010$x, 
    sexo = sexo,
    periodo = "2010-2019"
  )]
  descomposicion_resultados[[paste0("2010_2019_", sexo)]] <- descomp
}

# --- Período 2019-2021 ---
for (sexo in c("m", "f")) {
  sexo_full <- ifelse(sexo == "m", "male", "female")
  
  lt_2019 <- lt_output[year == 2019 & sex == sexo_full]
  lt_2021 <- lt_output[year == 2021 & sex == sexo_full]
  
  descomp <- descomposicion_arriaga(lt_2019, lt_2021)
  
  # Asignamos las variables y rescatamos la edad real desde la tabla de vida (column 'x')
  descomp$tabla_contribuciones[, `:=`(
    edad = lt_2019$x, 
    sexo = sexo,
    periodo = "2019-2021"
  )]
  descomposicion_resultados[[paste0("2019_2021_", sexo)]] <- descomp
}

# Consolidar todos los resultados en una sola tabla
tabla_descomposicion <- rbindlist(
  lapply(descomposicion_resultados, function(x) x$tabla_contribuciones)
)


# ==============================================================================
# 6. GENERACIÓN DE GRÁFICAS DE CONTRIBUCIÓN ----
# ==============================================================================

if(!dir.exists("images/ev")) dir.create("images/ev", recursive = TRUE)

ggplot(tabla_descomposicion, aes(x = edad, y = contribucion, fill = contribucion > 0)) +
  geom_col(width = 2.5, alpha = 0.8) +  
  facet_grid(sexo ~ periodo, labeller = as_labeller(c(
    "m" = "Hombres", 
    "f" = "Mujeres",
    "2010-2019" = "2010-2019",
    "2019-2021" = "2019-2021"
  ))) +
  scale_fill_manual(
    values = c("TRUE" = "#2c7fb8", "FALSE" = "#fa9fb5"),
    labels = c("TRUE" = "Aumento e₀", "FALSE" = "Disminución e₀"),
    name = "Contribución"
  ) +
  scale_x_continuous(
    breaks = seq(0, 85, by = 10),  
    limits = c(0, 85)
  ) +
  labs(
    title = "Contribución por Edad a los Cambios en Esperanza de Vida",
    subtitle = "Descomposición de Arriaga - Coahuila", 
    x = "Edad",
    y = "Contribución (años)",
    caption = "Fuente: Elaboración propia con datos INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    strip.background = element_rect(fill = "gray95", color = "gray80"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5)

# Guardar la gráfica final
ggsave("images/ev/cambios_de_ev.png", width = 10, height = 6, dpi = 300)


# ==============================================================================
# 7. GUARDAR RESULTADOS EN EXCEL ----
# ==============================================================================

if(!dir.exists("output")) dir.create("output", recursive = TRUE)

write.xlsx(
  list(
    descomposicion = tabla_descomposicion,
    resumen = data.table(
      periodo = rep(c("2010-2019", "2019-2021"), each = 2),
      sexo = rep(c("m", "f"), 2),
      dif_e0 = sapply(descomposicion_resultados, function(x) x$diferencia_e0)
    )
  ),
  "output/descomposicion_resultados.xlsx"
)
