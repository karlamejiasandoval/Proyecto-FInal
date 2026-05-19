# ==============================================================================
# Script 1: Preprocesamiento y Limpieza de Datos - Coahuila
# ==============================================================================

## Limpieza de graficos ----
graphics.off()

## Limpieza de memoria ----
rm(list = ls())

## Carga de paquetes y funciones ----
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

# ==============================================================================
# 1. PREPROCESAMIENTO CENSO 2010 ----
# ==============================================================================

c2010 <- read_excel("Data/INEGI_censo_2010.xls.xlsx", sheet = 1, 
                    range = "A7:D31")

names(c2010) <- c("age", "tot", "male", "female")
setDT(c2010)

c2010 <- c2010[-1 , ] 

# Limpieza de la variable de edad
c2010[ , age := gsub("De ", "", age)]
c2010[ , age := substr(age, 1, 2)]
c2010[age=="No", age:=NA]
c2010[ , age:=as.numeric(age)]

# Convertir a número quitando comas
c2010[ , tot := as.numeric(gsub(",", "", tot))]
c2010[ , male := as.numeric(gsub(",", "", male))]
c2010[ , female := as.numeric(gsub(",", "", female))]

# Control de NAs por coerción para Coahuila (Evita propagación de NAs)
c2010[is.na(tot), tot := 0]
c2010[is.na(male), male := 0]
c2010[is.na(female), female := 0]

# Agrupar las edades de 1 a 4 años en un solo bloque
c2010 <- c2010[ , age := ifelse(age %in% 1:4, 1, age)] %>% 
  .[ , .(tot = sum(tot), 
         male = sum(male), 
         female = sum(female)), .(age)]

# Reestructurar tabla (Melt)
c2010 <- melt.data.table(c2010, 
                         id.vars = "age",
                         measure.vars = c("male", "female"),
                         variable.name = "sex",
                         value.name = "pop")

c2010[ , year:=2010]


# ==============================================================================
# 2. PREPROCESAMIENTO CENSO 2020 ----
# ==============================================================================

c2020 <- read_excel("Data/INEGI_censo_2020.xls.xlsx", sheet = 1, 
                    range = "A7:D31")

names(c2020) <- c("age", "tot", "male", "female")
setDT(c2020)

c2020 <- c2020[-1 , ] 

# Limpieza de la variable de edad
c2020[ , age := gsub("De ", "", age)]
c2020[ , age := substr(age, 1, 2)]
c2020[age=="No", age:=NA]
c2020[ , age:=as.numeric(age)]

# Convertir a número quitando comas
c2020[ , tot := as.numeric(gsub(",", "", tot))]
c2020[ , male := as.numeric(gsub(",", "", male))]
c2020[ , female := as.numeric(gsub(",", "", female))]

# Control de NAs por coerción para Coahuila (Evita propagación de NAs)
c2020[is.na(tot), tot := 0]
c2020[is.na(male), male := 0]
c2020[is.na(female), female := 0]

# Agrupar las edades de 1 a 4 años en un solo bloque
c2020 <- c2020[ , age := ifelse(age %in% 1:4, 1, age)] %>% 
  .[ , .(tot = sum(tot), 
         male = sum(male), 
         female = sum(female)), .(age)]

# Reestructurar tabla (Melt)
c2020 <- melt.data.table(c2020, 
                         id.vars = "age",
                         measure.vars = c("male", "female"),
                         variable.name = "sex",
                         value.name = "pop")

c2020[ , year:=2020]


# ==============================================================================
# 3. UNIÓN DE CENSOS Y PRORRATEO DE EDAD NO ESPECIFICADA ----
# ==============================================================================

censos <- rbind(c2010, c2020)

censos_pro <- censos[ !is.na(age) ] %>% 
  .[ , p_pop := pop / sum(pop), .(year, sex)] %>% 
  merge( censos[ is.na(age), 
                 .(sex, year, na_pop=pop)], 
         by = c("sex", "year")) %>% 
  .[ , pop_adj := pop + na_pop * p_pop] %>% 
  .[ , .(year, sex, age, pop = pop_adj) ]

## Guardar tabla de censos corregida ----
write.csv(censos_pro, "Data/censos_pro.csv", row.names = FALSE)


# ==============================================================================
# 4. PREPROCESAMIENTO DE DEFUNCIONES 1990-2024 ----
# ==============================================================================

# Cambiamos col_names = FALSE para que la fila 6 sea tratada como datos y no se pierda 1990
def <- read_excel("Data/INEGI_deaths.xls.xlsx", sheet = 1, 
                  range = "A6:G9000", col_names = FALSE) 

names(def) <- c("age", "year", "reg", 
                "tot", "male", "female", "ns")
setDT(def)

## Filtro inicial ----
def <- def[age != "Total" & year != "Total"]
# Filtrar años válidos (ahora que year es fila de datos, eliminamos texto basura si lo hay)
def <- def[year %in% 1990:2026] 

## Limpieza de la edad ----
def[ , age := gsub("Menores de ", "", age)]
def[ , age := substr(age, 1, 2)]
def[age=="1 ", age:=0]
def[age=="1-", age:=1]
def[age=="5-", age:=5]
def[age=="No", age:=NA] 
def[ , age:=as.numeric(age)]

## Convertir a número las defunciones y controlar NAs por coerción ----
def[ , tot := as.numeric(gsub(",", "", tot))]
def[ , male := as.numeric(gsub(",", "", male))]
def[ , female := as.numeric(gsub(",", "", female))]
def[ , ns := as.numeric(gsub(",", "", ns))]

def[is.na(tot), tot := 0]
def[is.na(male), male := 0]
def[is.na(female), female := 0]
def[is.na(ns), ns := 0]

# Imputación de año no especificado utilizando región de registro
def[year=="No especificado", year:=reg] 
def[ , year:=as.numeric(year)] 

# Tabla resumen para prorrateo de defunciones por sexo
def_pro <- def[ , .(male=sum(male, na.rm = T), 
                    female=sum(female, na.rm = T),
                    ns=sum(ns, na.rm = T)), 
                .(year, age)]

# Prorrateo de los valores perdidos (missing) por sexo
def_pro <- def_pro[
  , tot := male + female
][
  tot > 0, `:=`(p_male = male/tot, p_female = female/tot)
][
  tot == 0, `:=`(p_male = 0.5, p_female = 0.5) # Caso extremo sin muertes
][
  , `:=`(male_adj = male + p_male*ns,
         female_adj = female + p_female*ns)
]

def_pro <- def_pro[ , .(year, age, male=male_adj, female=female_adj)]

def_pro <- melt.data.table(def_pro, 
                           id.vars = c("year", "age"),
                           measure.vars = c("male", "female"),
                           variable.name = "sex",
                           value.name = "deaths")

# --- PRORRATEO CONDICIONAL POR EDAD (Modificado para Coahuila) ---
def_validos <- def_pro[!is.na(age)]
def_nas <- def_pro[is.na(age), .(sex, year, na_deaths = deaths)]

# Si existen registros con edad No Especificada, se hace el prorrateo; si no, se mantiene intacta
if (nrow(def_nas) > 0) {
  def_validos[, p_deaths := deaths / sum(deaths), .(year, sex)]
  def_pro <- merge(def_validos, def_nas, by = c("sex", "year"), all.x = TRUE)
  def_pro[is.na(na_deaths), na_deaths := 0]
  def_pro[, deaths_adj := deaths + na_deaths * p_deaths]
  def_pro <- def_pro[, .(year, sex, age, deaths = deaths_adj)]
} else {
  def_pro <- def_validos[, .(year, sex, age, deaths)]
}

def_gr <- def_pro[ , .(deaths=sum(deaths)), .(year, sex)]

## Guardar tabla de defunciones prorrateadas ----
write.csv(def_pro, "Data/def_pro.csv", row.names = FALSE)

