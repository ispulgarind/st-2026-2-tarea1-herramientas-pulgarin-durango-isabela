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