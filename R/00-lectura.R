leer_serie <- function(x, fuente, unidad) {
  
  stopifnot(is.character(fuente))
  stopifnot(is.character(unidad))
  
  # CASO 1: x es un objeto ts
  
  if (is.ts(x)) {
    
    y <- as.numeric(x)
    
    if (any(is.na(y))) {
      stop("La serie contiene valores faltantes.")
    }
    
    frecuencia <- frequency(x)
    
    inicio <- start(x)
    
    if (frecuencia == 1) {
      
      fecha <- seq.Date(
        from = as.Date(paste0(inicio[1], "-01-01")),
        by = "year",
        length.out = length(y)
      )
      
    } else if (frecuencia == 4) {
      
      mes_inicial <- (inicio[2] - 1) * 3 + 1
      
      fecha <- seq.Date(
        from = as.Date(paste0(inicio[1], "-", mes_inicial, "-01")),
        by = "3 months",
        length.out = length(y)
      )
      
    } else if (frecuencia == 12) {
      
      fecha <- seq.Date(
        from = as.Date(paste0(inicio[1], "-", inicio[2], "-01")),
        by = "month",
        length.out = length(y)
      )
      
    } else {
      
      stop("La frecuencia del objeto ts debe ser 1, 4 o 12.")
      
    }
    
    # CASO 2: x es una ruta a un CSV
    
  } else if (is.character(x) && length(x) == 1) {
    
    datos <- read.csv(x)
    
    if (!all(c("fecha", "valor") %in% names(datos))) {
      stop("El CSV debe tener las columnas 'fecha' y 'valor'.")
    }
    
    fecha <- as.Date(datos$fecha)
    
    if (any(is.na(fecha))) {
      stop("Hay fechas inválidas en el CSV.")
    }
    
    if (length(fecha) < 2) {
      stop("Se necesitan al menos dos observaciones para verificar la frecuencia.")
    }
    
    y <- as.numeric(datos$valor)
    
    if (any(is.na(y))) {
      stop("La columna 'valor' contiene datos no numéricos o faltantes.")
    }
    
    if (any(diff(fecha) <= 0)) {
      stop("Las fechas deben estar en orden creciente.")
    }
    
    anio <- as.integer(format(fecha, "%Y"))
    mes <- as.integer(format(fecha, "%m"))
    
    indice_mes <- anio * 12 + mes
    
    if (all(diff(indice_mes) == 1)) {
      
      frecuencia <- 12
      
    } else if (all(diff(indice_mes) == 3)) {
      
      frecuencia <- 4
      
    } else if (all(diff(indice_mes) == 12)) {
      
      frecuencia <- 1
      
    } else {
      
      stop("Las fechas no tienen una frecuencia anual, trimestral o mensual constante.")
    }
    
  } else {
    
    stop("x debe ser un objeto ts o la ruta a un archivo CSV.")
    
  }
  
  # CREAR RESULTADO
  
  resultado <- tibble::tibble(
    t = 1:length(y),
    fecha = fecha,
    y = y
  )
  
  # AGREGAR ATRIBUTOS
  
  attr(resultado, "frecuencia") <- frecuencia
  attr(resultado, "fuente") <- fuente
  attr(resultado, "unidad") <- unidad
  
  return(resultado)
}
