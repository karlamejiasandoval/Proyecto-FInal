# ==============================================================================
# Script 8: Gráficas de Funciones de la Tabla de Mortalidad (lx y ex) - Coahuila
# ==============================================================================

# 1. Limpieza de memoria y gráficas ----
graphics.off()
rm(list = ls())

# 2. Carga de paquetes ----
library(data.table)
library(ggplot2)

# 3. Carga de las tablas de vida calculadas de Coahuila ----
tablas_vida <- fread("Data/tablas_vida_completas.csv")

# ==============================================================================
# 4. PREPROCESAMIENTO Y PARCHE DE EDADES ----
# ==============================================================================

# Si la columna de edad se llama 'x', la homologamos a 'age'
if ("x" %in% names(tablas_vida)) setnames(tablas_vida, "x", "age")

# Convertimos la edad a formato numérico puro (ej. "1-4" se vuelve 1, "85+" se vuelve 85)
tablas_vida[, age := as.numeric(gsub("-.*|\\+", "", as.character(age)))]

# Ordenamos perfectamente la base por año, sexo y edad
setorder(tablas_vida, year, sex, age)

# Aseguramos que exista la carpeta para guardar los nuevos gráficos
if(!dir.exists("images/funciones_vida")) dir.create("images/funciones_vida", recursive = TRUE)


# ==============================================================================
# 5. GRÁFICA 1: CURVA DE SUPERVIVENCIA (lx) ----
# ==============================================================================

ggplot(tablas_vida, aes(x = age, y = lx, color = sex)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  facet_wrap(~year) +
  scale_color_manual(
    values = c("male" = "#2c7fb8", "female" = "#fa9fb5"),
    labels = c("male" = "Hombres", "female" = "Mujeres"),
    name = "Sexo"
  ) +
  scale_x_continuous(breaks = seq(0, 85, by = 10)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Curva de Supervivencia (lx) - Coahuila",
    subtitle = "Número de sobrevivientes a edad exacta x de una cohorte teórica de 100,000 nacidos vivos",
    x = "Edad exacta (x)",
    y = "Sobrevivientes (lx)",
    caption = "Fuente: Elaboración propia con funciones actuariales basadas en datos de INEGI"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray30", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray95", color = "gray85"),
    strip.text = element_text(face = "bold")
  )

ggsave("images/funciones_vida/curva_supervivencia_lx.png", width = 10, height = 6, dpi = 300)


# ==============================================================================
# 6. GRÁFICA 2: CURVA DE ESPERANZA DE VIDA COMPLETA (ex) ----
# ==============================================================================

ggplot(tablas_vida, aes(x = age, y = ex, color = sex)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  facet_wrap(~year) +
  scale_color_manual(
    values = c("male" = "#2c7fb8", "female" = "#fa9fb5"),
    labels = c("male" = "Hombres", "female" = "Mujeres"),
    name = "Sexo"
  ) +
  scale_x_continuous(breaks = seq(0, 85, by = 10)) +
  labs(
    title = "Curva de Esperanza de Vida Remanente (ex) - Coahuila",
    subtitle = "Años promedio de vida que le restan por vivir a una persona a la edad exacta x",
    x = "Edad exacta (x)",
    y = "Esperanza de vida remanente (ex)",
    caption = "Fuente: Elaboración propia con funciones actuariales basadas en datos de INEGI"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray30", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray95", color = "gray85"),
    strip.text = element_text(face = "bold")
  )

ggsave("images/funciones_vida/curva_esperanza_ex.png", width = 10, height = 6, dpi = 300)

print("¡Gráficas de lx y ex generadas con éxito y guardadas en images/funciones_vida/!")

