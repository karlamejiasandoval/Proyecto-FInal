lt_abr <- function(x, mx, sex="f", IMR=NA){
  
  m <- length(x)
  n <- c(diff(x), NA)  
  
  ax <- n/2    
  
  # Pag. 4 notas de clase - cuadro
  
  ## Coale y Demeny edades 0 a 1
  
  if(sex=="m"){
    if(mx[1]>=0.107){ ax[1] <- 0.330 }else{
      ax[1] <- 0.045+2.684*mx[1]
    } 
  } else if(sex=="f"){
    if(mx[1]>=0.107){ ax[1] <- 0.350 }else{
      ax[1] <- 0.053+2.800*mx[1]
    }  
  }
  
  ## Coale y Demeny edades 1 a 4
  if(sex=="m"){
    if(mx[1]>=0.107){ ax[2] <- 1.352 }else{
      ax[2] <- 1.651-2.816*mx[1]
    } 
  } else if(sex=="f"){
    if(mx[1]>=0.107){ ax[2] <- 1.361 }else{
      ax[2] <- 1.522-1.518*mx[1]
    }  
  }
  
  # Probabilidad de muerte
  qx <- (n*mx)/(1+(n-ax)*mx)
  qx[m] <- 1
  
  # Proba de sobrevivir
  px <- 1-qx
  
  # l_x
  lx <- 100000 * cumprod(c(1,px[-m]))
  
  # Defunciones
  dx <- c(-diff(lx), lx[m])
  
  # Años persona vividos
  Lx <- n* c(lx[-1], 0) + ax*dx
  Lx[m] <- lx[m]/mx[m]
  
  # Años persona vividos acumulados
  
  Tx <- rev(cumsum(rev(Lx)))
  
  # Esperanza de vida
  ex <- Tx/lx
  
  return(data.table(x, n, mx, ax, qx, px, lx, dx, Lx, Tx, ex))
  
  
}

# Uso la función lt_abr
# lt_abr(x, mx)



# 3. Crecimiento exponencial ----

expo <- function(N_0, N_T, t_0, t_T, t){
  
  dt <- decimal_date(as.Date(t_T)) - decimal_date(as.Date(t_0))
  r <- log(N_T/N_0)/dt
  
  h <- t - decimal_date(as.Date(t_0))
  N_h <- N_0 * exp(r*h)  
  
  return(N_h)
  
}



# 3. Función de descomposición de Arriaga ----

descomposicion_arriaga <- function(lt1, lt2) {
  # Verificar que son data.tables
  if (!is.data.table(lt1)) lt1 <- as.data.table(lt1)
  if (!is.data.table(lt2)) lt2 <- as.data.table(lt2)
  
  # Verificar que las estructuras de edad coincidan
  if (!all(lt1$age == lt2$age)) {
    stop("Las estructuras de edad no coinciden entre las tablas")
  }
  
  n_edades <- nrow(lt1)
  contribuciones <- numeric(n_edades)
  
  # Constantes
  l0_1 <- lt1$lx[1]
  l0_2 <- lt2$lx[1]
  
  for (i in 1:n_edades) {
    # Término directo (efecto de la mortalidad a la edad x)
    term_directo <- (lt1$lx[i] / l0_1) * 
      ((lt2$Lx[i] / lt2$lx[i]) - (lt1$Lx[i] / lt1$lx[i]))
    
    # Término indirecto (efecto de la mortalidad en edades posteriores)
    if (i < n_edades) {
      term_indirecto <- (lt2$Tx[i + 1] / l0_2) * 
        ((lt1$lx[i] / lt2$lx[i]) - (lt1$lx[i + 1] / lt2$lx[i + 1]))
    } else {
      term_indirecto <- 0
    }
    
    contribuciones[i] <- term_directo + term_indirecto
  }
  
  # Verificación
  dif_e0 <- lt2$ex[1] - lt1$ex[1]
  suma_contrib <- sum(contribuciones)
  
  # Crear data.table con resultados
  resultado <- data.table(
    edad = lt1$age,
    contribucion = contribuciones,
    contribucion_porcentual = ifelse(dif_e0 != 0, (contribuciones / dif_e0) * 100, 0)
  )
  
  return(list(
    tabla_contribuciones = resultado,
    diferencia_e0 = dif_e0,
    suma_contribuciones = suma_contrib
  ))
}






# 4. Función para tabla de causa eliminada (homicidio) ----

tabla_causa_eliminada <- function(lt_todas_causas, prop_causa, causa_nombre = "causa_eliminada") {
  
  # Calcular mx sin la causa específica
  mx_sin_causa <- lt_todas_causas$mx * (1 - prop_causa)
  
  # Construir nueva tabla de vida sin la causa
  lt_sin_causa <- lt_abr(
    x = lt_todas_causas$x,
    mx = mx_sin_causa,
    sex = ifelse(mean(lt_todas_causas$mx) > 0.1, "m", "f") # Aproximación para determinar sexo
  )
  
  lt_sin_causa$causa <- paste0("sin_", causa_nombre)
  
  return(lt_sin_causa)
}
