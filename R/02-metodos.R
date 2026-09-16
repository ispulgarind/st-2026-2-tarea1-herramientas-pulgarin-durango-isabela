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






















