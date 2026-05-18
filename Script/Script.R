# 1. Instalar y cargar paquetes
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("data.table", dependencies = TRUE)
#install.packages("kableExtra")

# 2. Cargar paquetes
library(data.table)
library(ggplot2)
library(kableExtra)

# 3. Remover los objetos
rm(list = ls())

# 4. Descargar datos
pop <- fread("https://repodatos.atdt.gob.mx/CONAPO/proyecciones/00_Pob_Mitad_1950_2070.csv")


# 5. Filtrar Coahuila 2026
coah <- pop[ENTIDAD == "Coahuila" & ANIO == "2026",
            .(SEXO, EDAD, POBLACION)]

# 6. Preparar los datos para la pirámide
pop_coah<- pop[ENTIDAD == "Coahuila" & ANIO == 2026, .(SEXO, EDAD, POBLACION)]

# 7. Convertir población masculina a valores negativos para graficar
pop_coah[, POBLACION := ifelse(SEXO == "Hombres",-POBLACION, POBLACION)]

# 8. Graficar pirámide poblacional
ggplot(pop_coah, aes(x = EDAD, y = POBLACION, fill = SEXO)) +
  geom_bar(stat = "identity", width = 1) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(title = "Pirámide poblacional de Coahuila, 2026",
       x = "Edad",
       y = "Población") +
  theme_minimal() +
  scale_fill_manual(values = c("Hombres" = "steelblue", "Mujeres" = "pink"))

