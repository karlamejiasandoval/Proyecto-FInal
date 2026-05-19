# ==============================================================================
# Cálculo de Años Persona Vividos (APV) y Pirámides Poblacionales: Coahuila
# ==============================================================================

# Carga de paquetes ----
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

# Definición de la función expo (Incrustada para evitar el uso de archivos externos) ----
expo <- function(pop_0, pop_T, t_0, t_T, t) {
  # Convertir fechas a años decimales
  date_0 <- decimal_date(ymd(t_0))
  date_T <- decimal_date(ymd(t_T))
  
  # Tiempo transcurrido entre censos
  time_span <- date_T - date_0
  
  # Tasa de crecimiento exponencial (r)
  r <- log(pop_T / pop_0) / time_span
  
  # Estimación de la población al tiempo de interés 't'
  pop_t <- pop_0 * exp(r * (t - date_0))
  
  return(pop_t)
}

# Carga de tabla de datos ----
# (Asegúrate de haber corrido antes tu script de limpieza con los datos de Coahuila)
censos_pro <- fread("Data/censos_pro.csv")


# ==============================================================================
# 1. CÁLCULO DE APV (POBLACIÓN A MITAD DE AÑO) ----
# ==============================================================================

# --- Año 2010 ---
N_2010 <- expo(censos_pro[year==2010] %>% .$pop, 
               censos_pro[year==2020] %>% .$pop, 
               t_0 = "2010-06-25", t_T = "2020-03-15", t = 2010.5)

apv2010 <- censos_pro[year==2010, .(age, sex)]
apv2010[, `:=`(N = N_2010, year = 2010)]


# --- Año 2019 ---
N_2019 <- expo(censos_pro[year==2010] %>% .$pop, 
               censos_pro[year==2020] %>% .$pop, 
               t_0 = "2010-06-25", t_T = "2020-03-15", t = 2019.5)

apv2019 <- censos_pro[year==2020, .(age, sex)]
apv2019[, `:=`(N = N_2019, year = 2019)]


# --- Año 2021 ---
N_2021 <- expo(censos_pro[year==2010] %>% .$pop, 
               censos_pro[year==2020] %>% .$pop, 
               t_0 = "2010-06-25", t_T = "2020-03-15", t = 2021.5)

apv2021 <- censos_pro[year==2020, .(age, sex)]
apv2021[, `:=`(N = N_2021, year = 2021)]


# --- Consolidar y guardar tablas históricas de APV ---
apv <- rbind(apv2010, apv2019, apv2021)
write.csv(apv, "Data/apv.csv", row.names = FALSE)


# ==============================================================================
# 2. PROCESAMIENTO QUINQUENAL PARA GRÁFICOS ----
# ==============================================================================

apv_quinquenal <- copy(apv)

# Definir grupos quinquenales estándares (0-4, 5-9, ..., 85+)
apv_quinquenal[, age_group := cut(age, 
                                  breaks = c(0, seq(5, 85, by = 5), Inf),
                                  labels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                             "25-29", "30-34", "35-39", "40-44", "45-49",
                                             "50-54", "55-59", "60-64", "65-69", "70-74",
                                             "75-79", "80-84", "85+"),
                                  right = FALSE)]

# Sumar la población (N) por año, grupo quinquenal y sexo
apv_quinquenal <- apv_quinquenal[, .(N = sum(N, na.rm = TRUE)), by = .(year, age_group, sex)]

# Asegurar el orden correcto de los factores de edad en el eje
apv_quinquenal$age_group <- factor(apv_quinquenal$age_group, 
                                   levels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                              "25-29", "30-34", "35-39", "40-44", "45-49",
                                              "50-54", "55-59", "60-64", "65-69", "70-74",
                                              "75-79", "80-84", "85+"))


# ==============================================================================
# 3. GENERACIÓN DE PIRÁMIDES POBLACIONALES (COAHUILA) ----
# ==============================================================================

# Asegurar que existan las carpetas de destino para que ggsave no falle
if(!dir.exists("images/apv")) dir.create("images/apv", recursive = TRUE)

# --- Gráfica Coahuila 2010 ---
ggplot(apv_quinquenal[year == 2010], aes(x = age_group, y = ifelse(sex == "male", -N/1e6, N/1e6), fill = sex)) +
  geom_col(width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("male" = "#2c7fb8", "female" = "#fa9fb5"),
    labels = c("male" = "Hombres", "female" = "Mujeres")
  ) +
  labs(
    title = "Pirámide Poblacional de Coahuila 2010",
    subtitle = "Distribución por grupos quinquenales de edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (millones)",    
    fill = "Sexo",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )
ggsave("images/apv/apv_2010.png", width = 10, height = 6, dpi = 300)


# --- Gráfica Coahuila 2019 ---
ggplot(apv_quinquenal[year == 2019], aes(x = age_group, y = ifelse(sex == "male", -N/1e6, N/1e6), fill = sex)) +
  geom_col(width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("male" = "#2c7fb8", "female" = "#fa9fb5"),
    labels = c("male" = "Hombres", "female" = "Mujeres")
  ) +
  labs(
    title = "Pirámide Poblacional de Coahuila 2019",
    subtitle = "Distribución por grupos quinquenales de edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (millones)",    
    fill = "Sexo",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )
ggsave("images/apv/apv_2019.png", width = 10, height = 6, dpi = 300)


# --- Gráfica Coahuila 2021 ---
ggplot(apv_quinquenal[year == 2021], aes(x = age_group, y = ifelse(sex == "male", -N/1e6, N/1e6), fill = sex)) +
  geom_col(width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("male" = "#2c7fb8", "female" = "#fa9fb5"),
    labels = c("male" = "Hombres", "female" = "Mujeres")
  ) +
  labs(
    title = "Pirámide Poblacional de Coahuila 2021",
    subtitle = "Distribución por grupos quinquenales de edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (millones)",    
    fill = "Sexo",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )
ggsave("images/apv/apv_2021.png", width = 10, height = 6, dpi = 300)
