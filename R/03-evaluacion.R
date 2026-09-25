# MEDIDAS DE ERROR

medidas <- function(y, yhat, s = 1) {
  
  stopifnot(is.numeric(y))
  stopifnot(is.numeric(yhat))
  stopifnot(length(s) == 1, s >= 1, s == as.integer(s))
  
  if (any(is.na(y))) {
    stop("La serie y contiene valores faltantes.")
  }
  
  if (any(is.na(yhat))) {
    stop("La serie yhat contiene valores faltantes.")
  }
  
  if (length(y) != length(yhat)) {
    stop("y y yhat deben tener la misma longitud.")
  }
  
  T <- length(y)
  
  if (s >= T) {
    stop("El período s debe ser menor que la longitud de la serie.")
  }
  
  
  # Error de pronóstico
  e <- y - yhat
  
  
  # MSE
  MSE <- (1 / length(e)) * sum(e^2)
  
  
  # MAD
  MAD <- (1 / length(e)) * sum(abs(e))
  
  
  # MAPE
  if (any(y == 0)) {
    MAPE <- NA_real_
  } else {
    MAPE <- (100 / length(e)) *
      sum(abs(e) / abs(y))
  }
  
  
  # MASE
  error_ingenuo <- abs(
    y[(s + 1):T] - y[1:(T - s)]
  )
  
  escala <- (1 / (T - s)) *
    sum(error_ingenuo)
  
  if (escala == 0) {
    MASE <- NA_real_
  } else {
    MASE <- MAD / escala
  }
  
  
  return(
    list(
      MSE = MSE,
      MAD = MAD,
      MAPE = MAPE,
      MASE = MASE
    )
  )
}


#FUNCIÓN JUNG BOX
ljung_box <- function(r, T, m, p) {
  
  stopifnot(is.numeric(r))
  
  stopifnot(is.numeric(T))
  stopifnot(is.numeric(m))
  stopifnot(is.numeric(p))
  
  if (any(is.na(r))) {
    stop("La ACF contiene valores faltantes.")
  }
  
  if (T <= 0) {
    stop("T debe ser mayor que 0.")
  }
  
  if (m < 1 || m >= T) {
    stop("m debe ser mayor que 0 y menor que T.")
  }
  
  if (length(r) < m) {
    stop("r no contiene suficientes autocorrelaciones.")
  }
  
  if (p < 0 || p >= m) {
    stop("p debe ser mayor o igual que 0 y menor que m.")
  }
  
  # Calcular el estadístico de Ljung-Box
  Q <- T * (T + 2) *
    sum(r[1:m]^2 / (T - (1:m)))
  
  # Grados de libertad
  gl <- m - p
  
  # Valor crítico al 5 %
  valor_critico <- stats::qchisq(0.95, df = gl)
  
  # Valor p
  valor_p <- stats::pchisq(
    Q,
    df = gl,
    lower.tail = FALSE
  )
  
  return(
    list(
      estadistico = Q,
      grados_libertad = gl,
      valor_critico = valor_critico,
      valor_p = valor_p
    )
  )
}


# FUNCIÓN JARQUE BERA
jarque_bera <- function(e) {
  
  stopifnot(is.numeric(e))
  
  if (any(is.na(e))) {
    stop("Los errores contienen valores faltantes.")
  }
  
  N <- length(e)
  
  if (N < 3) {
    stop("Se necesitan al menos 3 observaciones.")
  }
  
  media <- mean(e)
  
  # Calcular los momentos centrales
  momento_2 <- mean((e - media)^2)
  momento_3 <- mean((e - media)^3)
  momento_4 <- mean((e - media)^4)
  
  # Calcular asimetría
  A <- momento_3 / momento_2^(3/2)
  
  # Calcular curtosis
  K <- momento_4 / momento_2^2
  
  # Calcular el estadístico Jarque-Bera
  JB <- N / 6 * (A^2 + (K - 3)^2 / 4)
  
  # Grados de libertad
  gl <- 2
  
  # Valor crítico al 5 %
  valor_critico <- stats::qchisq(0.95, df = gl)
  
  # Valor p
  valor_p <- stats::pchisq(
    JB,
    df = gl,
    lower.tail = FALSE
  )
  
  return(
    list(
      estadistico = JB,
      grados_libertad = gl,
      valor_critico = valor_critico,
      valor_p = valor_p
    )
  )
}


#FUNCIÓN DURBIN WATSON
durbin_watson <- function(e) {
  
  stopifnot(is.numeric(e))
  
  if (any(is.na(e))) {
    stop("Los errores contienen valores faltantes.")
  }
  
  N <- length(e)
  
  if (N < 2) {
    stop("Se necesitan al menos 2 observaciones.")
  }
  
  if (sum(e^2) == 0) {
    stop("El denominador es cero.")
  }
  
  # Calcular el estadístico Durbin-Watson
  d <- sum(
    (e[2:N] - e[1:(N - 1)])^2
  ) / sum(e^2)
  
  return(
    list(
      estadistico = d,
      grados_libertad = NA,
      valor_critico = NA,
      valor_p = NA
    )
  )
}


# VALIDACIÓN DE ERRORES

validar_errores <- function(e, m = NULL, p = 0) {
  
  stopifnot(is.numeric(e))
  stopifnot(length(p) == 1, p >= 0, p == as.integer(p))
  
  if (any(is.na(e))) {
    stop("El vector de errores contiene valores faltantes.")
  }
  
  n <- length(e)
  
  if (n < 3) {
    stop("Se necesitan al menos tres errores.")
  }
  
  
  if (is.null(m)) {
    m <- min(floor(n / 4), 24)
  }
  
  if (m < 1 || m >= n) {
    stop("El número de rezagos debe ser mayor que 0 y menor que el número de errores.")
  }
  
  
  # GRÁFICO DE LOS ERRORES
  
  datos_errores <- data.frame(
    t = 1:n,
    error = e
  )
  
  grafico_errores <- ggplot2::ggplot(
    datos_errores,
    ggplot2::aes(x = t, y = error)
  ) +
    ggplot2::geom_line(
      color = "purple"
    ) +
    ggplot2::geom_hline(
      yintercept = 0
    ) +
    ggplot2::labs(
      title = "Errores de pronóstico",
      x = "Tiempo",
      y = "Error"
    ) +
    ggplot2::theme_minimal()
  
  
  # CORRELOGRAMA DE LOS ERRORES
  
  correlograma_errores <- correlograma(
    e,
    m
  )
  
  # PRUEBA T DE MEDIA CERO
  
  media <- mean(e)
  desviacion <- sd(e)
  
  estadistico_t <- media / (desviacion / sqrt(n))
  
  grados_libertad_t <- n - 1
  
  valor_p_t <- 2 * pt(
    -abs(estadistico_t),
    df = grados_libertad_t
  )
  
  
  # LJUNG-BOX
  
  resultado_ljung <- ljung_box(
    r = correlograma_errores$acf,
    T = n,
    m = m,
    p = p
  )
  
  # JARQUE-BERA
  
  resultado_jb <- jarque_bera(e)
  
  
  # DURBIN-WATSON
  
  resultado_dw <- durbin_watson(e)
  
  return(
    list(
      errores = e,
      
      grafico_errores = grafico_errores,
      
      correlograma = correlograma_errores,
      
      media = media,
      
      t = estadistico_t,
      
      grados_libertad_t = grados_libertad_t,
      
      valor_p_t = valor_p_t,
      
      ljung_box = resultado_ljung,
      
      jarque_bera = resultado_jb,
      
      durbin_watson = resultado_dw
    )
  )
}
