# FUNCIÓN PARA GRAFICAR UNA SERIE DE TIEMPO
graficar_serie <- function(datos, titulo) {
  
  # Crear el gráfico usando la fecha en el eje X y los valores en el eje Y
  ggplot2::ggplot(datos, ggplot2::aes(x = fecha, y = y)) +
    
    # Dibujar la serie como una línea
    ggplot2::geom_line() +
    
    # Agregar título, nombres de los ejes y caption
    ggplot2::labs(
      title = titulo,
      x = "Fecha",
      y = paste0("Valor (", attr(datos, "unidad"), ")"),
      caption = paste0(
        "Fuente: ", attr(datos, "fuente"),
        " | Observaciones: ", length(datos$y)
      )
    ) +
    
    # Usar un diseño sencillo y limpio
    ggplot2::theme_minimal()
}



#FUNCIÓN PARA CALCULAR Y GRAFICAR UN CORRELOGRAMA

correlograma <- function(datos, m = NULL) {
  
  stopifnot(is.numeric(datos))
  
  if (any(is.na(datos))) {
    stop("La serie contiene valores faltantes.")
  }
  
  # Número de observaciones
  T <- length(datos)
  
  if (T < 2) {
    stop("La serie debe tener al menos dos observaciones.")
  }
  
  # Definir el número de rezagos
  
  if (is.null(m)) {
    m <- min(floor(T / 4), 24)
  }
  
  # Verificar que m sea válido
  if (m < 1 || m >= T) {
    stop("El número de rezagos debe ser mayor que 0 y menor que T.")
  }
  
  # Calcular la media
  
  media <- mean(datos)
  
  # Calcular el divisor único
  
  divisor <- sum((datos - media)^2)
  
  # Calcular la ACF a mano
  
  acf_mano <- numeric(m)
  
  for (h in 1:m) {
    
    # Numerador de la autocorrelación en el rezago h
    numerador <- sum(
      (datos[(h + 1):T] - media) *
        (datos[1:(T - h)] - media)
    )
    
    # Autocorrelación usando el mismo divisor para todos los rezagos
    acf_mano[h] <- numerador / divisor
  }
  
  # Calcular la PACF usando pacf() de R
  
  pacf_r <- stats::pacf(
    datos,
    lag.max = m,
    plot = FALSE
  )
  
  pacf_valores <- as.numeric(pacf_r$acf)
  
  # Calcular la banda de confianza
  
  banda <- stats::qnorm((1 + 0.95) / 2) / sqrt(T)
  
  # Crear datos para el gráfico de ACF
  
  datos_acf <- data.frame(
    rezago = 1:m,
    valor = acf_mano
  )
  
  # Crear datos para el gráfico de PACF
  
  datos_pacf <- data.frame(
    rezago = 1:m,
    valor = pacf_valores
  )
  
  # Gráfico de ACF
  
  grafico_acf <- ggplot2::ggplot(
    datos_acf,
    ggplot2::aes(x = rezago, y = valor)
  ) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_hline(
      yintercept = c(-banda, banda),
      linetype = "dashed"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        xend = rezago,
        y = 0,
        yend = valor
      )
    ) +
    ggplot2::labs(
      title = "ACF",
      x = "Rezago",
      y = "Autocorrelación"
    ) +
    ggplot2::theme_minimal()
  
  # Gráfico de PACF
  
  grafico_pacf <- ggplot2::ggplot(
    datos_pacf,
    ggplot2::aes(x = rezago, y = valor)
  ) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_hline(
      yintercept = c(-banda, banda),
      linetype = "dashed"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        xend = rezago,
        y = 0,
        yend = valor
      )
    ) +
    ggplot2::labs(
      title = "PACF",
      x = "Rezago",
      y = "Autocorrelación parcial"
    ) +
    ggplot2::theme_minimal()
  
  # Unir los dos gráficos
  
  panel <- patchwork::wrap_plots(
    grafico_acf,
    grafico_pacf,
    ncol = 1
  )
  
  # Devolver los resultados
  
  return(
    list(
      acf = acf_mano,
      pacf = pacf_valores,
      banda = banda,
      grafico = panel
    )
  )
}



# VERIFICACIÓN DE LA ACF

datos_prueba <- c(18.43, 19.98, 19.51, 20.03, 19.78, 21.25, 21.18, 22.14)

resultado <- correlograma(datos_prueba, m = 5)

# ACF manual: r_h
rh_manual <- resultado$acf

# ACF calculada por R
acf_r <- stats::acf(
  datos_prueba,
  lag.max = 5,
  plot = FALSE
)

# Valores de R para los rezagos
rh_r <- as.numeric(acf_r$acf[-1])

# Máxima diferencia absoluta entre ambas
diferencia <- max(abs(rh_manual - rh_r))

cat("Máxima diferencia absoluta:", diferencia, "\n")

stopifnot(diferencia < 10^(-12))


# Verificación de banda
banda_nuestra <- resultado$banda

banda_r <- stats::qnorm((1 + 0.95) / 2) / sqrt(length(datos_prueba))

diferencia_banda <- abs(banda_nuestra - banda_r)

cat("Banda de nuestra función:", banda_nuestra, "\n")
cat("Banda de R:", banda_r, "\n")
cat("Diferencia entre las bandas:", diferencia_banda, "\n")

stopifnot(diferencia_banda < 10^(-12))
