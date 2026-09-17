# MEDIA SIMPLE

ajustar_media <- function(y) {
  
  stopifnot(is.numeric(y))
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  if (length(y) < 2) {
    stop("Se necesitan al menos 2 observaciones.")
  }
  
  T <- length(y)
  
  yhat <- rep(NA_real_, T)
  
  yhat[2] <- y[1]
  
  suma <- y[1]
  
  for (t in 2:(T - 1)) {
    
    suma <- suma + y[t]
    
    yhat[t + 1] <- suma / t
  }
  
  pronosticar <- function(h) {
    
    stopifnot(length(h) == 1, h >= 1, h == as.integer(h))
    
    rep(mean(y), h)
  }
  
  return(
    list(
      yhat = yhat,
      pronosticar = pronosticar,
      parametros = list()
    )
  )
}



# MEDIA MÓVIL

ajustar_mm <- function(y, k) {
  
  stopifnot(is.numeric(y))
  stopifnot(length(k) == 1, k >= 2, k == as.integer(k))
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  T <- length(y)
  
  if (k > T) {
    stop("La ventana k no puede ser mayor que la longitud de la serie.")
  }
  
  yhat <- rep(NA_real_, T)
  
  suma <- sum(y[1:k])
  
  if (k < T) {
    yhat[k + 1] <- suma / k
  }
  
  if (T > k + 1) {
    
    for (t in (k + 1):(T - 1)) {
      
      suma <- suma - y[t - k]
      
      suma <- suma + y[t]
      
      yhat[t + 1] <- suma / k
    }
  }
  
  # Pronósticos fuera de la muestra
  pronosticar <- function(h) {
    
    stopifnot(length(h) == 1, h >= 1, h == as.integer(h))
    
    ultimo_mm <- mean(y[(T - k + 1):T])
    
    rep(ultimo_mm, h)
  }
  
  return(
    list(
      yhat = yhat,
      pronosticar = pronosticar,
      parametros = list(k = k)
    )
  )
}



# SUAVIZAMIENTO EXPONENCIAL SIMPLE

ajustar_ses <- function(y, alpha) {
  
  stopifnot(is.numeric(y))
  stopifnot(length(alpha) == 1)
  
  if (alpha <= 0 || alpha >= 1) {
    stop("alpha debe estar entre 0 y 1.")
  }
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  if (length(y) < 2) {
    stop("Se necesitan al menos 2 observaciones.")
  }
  
  T <- length(y)
  
  yhat <- rep(NA_real_, T)
  
  yhat[2] <- y[1]
  
  if (T > 2) {
    
    for (t in 2:(T - 1)) {
      
      yhat[t + 1] <- alpha * y[t] +
        (1 - alpha) * yhat[t]
    }
  }
  
  pronosticar <- function(h) {
    
    stopifnot(length(h) == 1, h >= 1, h == as.integer(h))
    
    ultimo_pronostico <- alpha * y[T] +
      (1 - alpha) * yhat[T]
    
    rep(ultimo_pronostico, h)
  }
  
  return(
    list(
      yhat = yhat,
      pronosticar = pronosticar,
      parametros = list(alpha = alpha)
    )
  )
}



# DOBLE MEDIA MÓVIL

ajustar_dmm <- function(y, k) {
  
  stopifnot(is.numeric(y))
  stopifnot(length(k) == 1, k >= 2, k == as.integer(k))
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  T <- length(y)
  
  if (k > T) {
    stop("La ventana k no puede ser mayor que la longitud de la serie.")
  }
  
  if (2 * k - 1 > T) {
    stop("Se necesitan al menos 2k - 1 observaciones.")
  }
  
  #CALCULAR LA PRIMERA MEDIA MÓVIL
  
  MM <- rep(NA_real_, T)
  
  suma <- sum(y[1:k])
  MM[k] <- suma / k
  
  if (T > k) {
    
    for (t in (k + 1):T) {
      
      suma <- suma - y[t - k]
      suma <- suma + y[t]
      
      MM[t] <- suma / k
    }
  }
  

  #CALCULAR LA SEGUNDA MEDIA MÓVIL

  DMM <- rep(NA_real_, T)
  
  suma_mm <- sum(MM[k:(2 * k - 1)])
  DMM[2 * k - 1] <- suma_mm / k
  
  if (T > 2 * k - 1) {
    
    for (t in (2 * k):T) {
      
      suma_mm <- suma_mm - MM[t - k]
      suma_mm <- suma_mm + MM[t]
      
      DMM[t] <- suma_mm / k
    }
  }
  
  #CALCULAR NIVEL Y TENDENCIA
  
  E <- rep(NA_real_, T)
  beta1 <- rep(NA_real_, T)
  
  for (t in (2 * k - 1):T) {
    
    # Nivel estimado
    E[t] <- 2 * MM[t] - DMM[t]
    
    # Tendencia estimada
    beta1[t] <- 2 / (k - 1) *
      (MM[t] - DMM[t])
  }
  
  
  yhat <- rep(NA_real_, T)
  
  for (t in (2 * k - 1):(T - 1)) {
    
    yhat[t + 1] <- E[t] + beta1[t]
  }
  
  
  pronosticar <- function(h) {
    
    stopifnot(length(h) == 1, h >= 1, h == as.integer(h))
    
    pronosticos <- numeric(h)
    
    for (j in 1:h) {
      
      pronosticos[j] <- E[T] + beta1[T] * j
    }
    
    return(pronosticos)
  }
  
  return(
    list(
      yhat = yhat,
      pronosticar = pronosticar,
      parametros = list(
        k = k,
        E = E[T],
        beta1 = beta1[T],
        trayectoria_E = E,
        trayectoria_beta1 = beta1
      )
    )
  )
}


# Función auxiliar para calcular la información estadística de una regresión por mínimos cuadrados

resumen_regresion <- function(X, y, residuos) {
  
  T <- length(y)
  
  XtX <- crossprod(X)
  
  XtX_inv <- solve(XtX)
  
  p <- ncol(X)
  sigma2 <- sum(residuos^2) / (T - p)
  
  # Matriz de varianza ordinaria de los coeficientes
  var_ordinaria <- sigma2 * XtX_inv
  
  #Error estándar ordinario
  se_ordinario <- sqrt(diag(var_ordinaria))
  
  
  # ERROR ESTÁNDAR ROBUSTO HAC
  
  # Número de rezagos para el núcleo de Bartlett
  L <- floor(4 * (T / 100)^(2 / 9))
  
  S <- matrix(0, nrow = p, ncol = p)
  
  # Rezago 0
  for (t in 1:T) {
    xt <- X[t, ]
    S <- S + residuos[t]^2 * tcrossprod(xt)
  }
  
  # Demás rezagos
  if (L > 0) {
    
    for (h in 1:L) {
      
      # Peso del núcleo de Bartlett
      peso <- 1 - h / (L + 1)
      
      for (t in (h + 1):T) {
        
        xt <- X[t, ]
        xh <- X[t - h, ]
        
        S <- S +
          peso * residuos[t] * residuos[t - h] *
          (tcrossprod(xt, xh) + tcrossprod(xh, xt))
      }
    }
  }
  
  # Matriz de varianza robusta
  var_robusta <- XtX_inv %*% S %*% XtX_inv
  
  # Errores estándar robustos
  se_robusto <- sqrt(diag(var_robusta))
  
  
  # ESTADÍSTICO t Y VALOR p
  
  beta <- solve(XtX, crossprod(X, y))
  
  t_ordinario <- beta / se_ordinario
  t_robusto <- beta / se_robusto
  
  p_robusto <- 2 * pt(
    -abs(t_robusto),
    df = T - p
  )
  
  
  # R2
  
  y_ajustado <- X %*% beta
  
  suma_total <- sum((y - mean(y))^2)
  suma_residuos <- sum(residuos^2)
  
  R2 <- 1 - suma_residuos / suma_total
  
  
  # DURBIN-WATSON
  
  DW <- sum(diff(residuos)^2) / sum(residuos^2)
  
  
  # Tabla de coeficientes
  tabla <- data.frame(
    estimacion = as.numeric(beta),
    se_ordinario = se_ordinario,
    se_robusto = se_robusto,
    t = t_robusto,
    valor_p = p_robusto
  )
  
  return(
    list(
      beta = as.numeric(beta),
      tabla = tabla,
      R2 = R2,
      sigma2 = sigma2,
      DW = DW,
      rezagos_HAC = L,
      residuos = residuos,
      y_ajustado = as.numeric(y_ajustado)
    )
  )
}


#AJUSTAR TENDENCIA

ajustar_tendencia <- function(y, tipo) {
  
  stopifnot(is.numeric(y))
  stopifnot(length(tipo) == 1)
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  if (!tipo %in% c("lineal", "cuadratica", "exponencial")) {
    stop("El tipo debe ser 'lineal', 'cuadratica' o 'exponencial'.")
  }
  
  # Número de observaciones
  T <- length(y)
  
  if (T < 3) {
    stop("Se necesitan al menos 3 observaciones.")
  }
  
  
  # TENDENCIA LINEAL
  
  if (tipo == "lineal") {
    
    t <- 1:T
    
    X <- cbind(
      1,
      t
    )
    
    # Estimar los coeficientes
    beta <- solve(
      crossprod(X),
      crossprod(X, y)
    )
    
    yhat <- as.numeric(X %*% beta)
    
    residuos <- y - yhat
    
    resumen <- resumen_regresion(
      X,
      y,
      residuos
    )
    
    pronosticar <- function(h) {
      
      stopifnot(
        length(h) == 1,
        h >= 1,
        h == as.integer(h)
      )
      
      t_futuro <- T + (1:h)
      
      pronosticos <- beta[1] + beta[2] * t_futuro
      
      return(as.numeric(pronosticos))
    }
    
    return(
      list(
        yhat = yhat,
        pronosticar = pronosticar,
        parametros = list(
          tipo = "lineal",
          coeficientes = resumen$tabla,
          R2 = resumen$R2,
          sigma2 = resumen$sigma2,
          DW = resumen$DW,
          rezagos_HAC = resumen$rezagos_HAC
        )
      )
    )
  }
}










