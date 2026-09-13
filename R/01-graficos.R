# FUNCIÓN PARA GRAFICAR UNA SERIE DE TIEMPO
graficar_serie <- function(datos, titulo) {
  
  ggplot2::ggplot(datos, ggplot2::aes(x = fecha, y = y)) +
    
    ggplot2::geom_line() +
    
    ggplot2::labs(
      title = titulo,
      x = "Fecha",
      y = paste0("Valor (", attr(datos, "unidad"), ")"),
      caption = paste0(
        "Fuente: ", attr(datos, "fuente"),
        " | Observaciones: ", length(datos$y)
      )
    ) +
    
    ggplot2::theme_minimal()
}



#FUNCIÓN PARA CALCULAR Y GRAFICAR UN CORRELOGRAMA

correlograma <- function(datos, m = NULL) {
  
  stopifnot(is.numeric(datos))
  
  if (any(is.na(datos))) {
    stop("La serie contiene valores faltantes.")
  }
  
  T <- length(datos)
  
  if (T < 2) {
    stop("La serie debe tener al menos dos observaciones.")
  }
  
  
  if (is.null(m)) {
    m <- min(floor(T / 4), 24)
  }
  
  if (m < 1 || m >= T) {
    stop("El número de rezagos debe ser mayor que 0 y menor que T.")
  }
  
  
  media <- mean(datos)
  
  
  divisor <- sum((datos - media)^2)
  
  # Calcular la ACF
  
  acf_mano <- numeric(m)
  
  for (h in 1:m) {
    
    numerador <- sum(
      (datos[(h + 1):T] - media) *
        (datos[1:(T - h)] - media)
    )
    
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




