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


