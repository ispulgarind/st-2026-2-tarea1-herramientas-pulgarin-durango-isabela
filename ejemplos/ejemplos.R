# EJEMPLOS Y VERIFICACIONES

#VERIFICACIÓN DE LAS FUNCIONES

#CARGA DE FUNCIONES
source("R/00-lectura.R")
source("R/01-graficos.R")
source("R/02-metodos.R")
source("R/03-evaluacion.R")

#CORRELOGRAMA

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
mi_banda <- resultado$banda

banda_r <- stats::qnorm((1 + 0.95) / 2) / sqrt(length(datos_prueba))

diferencia_banda <- abs(mi_banda - banda_r)

cat("Banda de mi función:", mi_banda, "\n")
cat("Banda de R:", banda_r, "\n")
cat("Diferencia entre las bandas:", diferencia_banda, "\n")

stopifnot(diferencia_banda < 10^(-12))


#EVALUACIÓN

#VERIFICACIÓN JUNG BOX 

r <- c(0.30, 0.20, 0.10, 0.05, 0.02)

T <- 100
m <- 5
p <- 0

mi_resultado <- ljung_box(r, T, m, p)

# Verificación contra Box.test()
datos_prueba <- c(
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
  21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
  31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
  41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
  51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
  61, 62, 63, 64, 65, 66, 67, 68, 69, 70,
  71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
  81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
  91, 92, 93, 94, 95, 96, 97, 98, 99, 100
)

# Obtener la ACF para usar nuestra función
acf_prueba <- stats::acf(
  datos_prueba,
  lag.max = m,
  plot = FALSE
)

r_prueba <- as.numeric(acf_prueba$acf[-1])

# Nuestro Ljung-Box
mi_resultado <- ljung_box(
  r = r_prueba,
  T = length(datos_prueba),
  m = m,
  p = p
)

# Ljung-Box de R
resultado_r <- stats::Box.test(
  datos_prueba,
  lag = m,
  type = "Ljung-Box",
  fitdf = p
)

# Comparar los estadísticos
diferencia <- abs(
  mi_resultado$estadistico -
    as.numeric(resultado_r$statistic)
)

cat(
  "Estadístico de mi función:",
  mi_resultado$estadistico,
  "\n"
)

cat(
  "Estadístico de Box.test():",
  as.numeric(resultado_r$statistic),
  "\n"
)

cat(
  "Máxima diferencia absoluta:",
  diferencia,
  "\n"
)

stopifnot(diferencia < 10^(-12))



#EJEMPLOS DE SERIES DE TIEMPO