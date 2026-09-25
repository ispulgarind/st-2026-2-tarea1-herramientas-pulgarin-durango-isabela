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



# VERIFICACIÓN TENDENCIA LINEAL

y <- c(10, 12, 13, 15, 16, 18, 20, 21)
t <- 1:length(y)

modelo_manual <- ajustar_tendencia(y, "lineal")

modelo_lm <- lm(y ~ t)

coef(modelo_lm)
modelo_manual$parametros$coeficientes$estimacion



# VERIFICACIÓN TENDENCIA CUADRÁTICA

modelo_manual <- ajustar_tendencia(y, "cuadratica")

modelo_lm <- lm(y ~ t + I(t^2))

coef(modelo_lm)
modelo_manual$parametros$coeficientes$estimacion



# VERIFICACIÓN TENDENCIA EXPONENCIAL

y_exp <- c(10, 12, 15, 18, 22, 27, 33, 40)
t <- 1:length(y_exp)

modelo_manual <- ajustar_tendencia(y_exp, "exponencial")

modelo_lm <- lm(log(y_exp) ~ t)

coef(modelo_lm)
modelo_manual$parametros$coeficientes$estimacion



# VERIFICACIÓN DE HOLT LINEAL

y <- c(10, 12, 13, 15, 16, 18, 20, 21)

alpha <- 0.3
beta <- 0.2

modelo_holt <- ajustar_holt(
  y,
  alpha,
  beta
)

L <- modelo_holt$parametros$L
b <- modelo_holt$parametros$b
yhat <- modelo_holt$yhat


e <- y - yhat

L_verificacion <- rep(NA_real_, length(y))

for (t in 2:length(y)) {
  
  L_verificacion[t] <- L[t - 1] +
    b[t - 1] +
    alpha * e[t]
}


b_verificacion <- rep(NA_real_, length(y))

for (t in 2:length(y)) {
  
  b_verificacion[t] <- b[t - 1] +
    alpha * beta * e[t]
}


diferencia_L <- max(
  abs(L[2:length(y)] - L_verificacion[2:length(y)])
)

diferencia_b <- max(
  abs(b[2:length(y)] - b_verificacion[2:length(y)])
)

print(diferencia_L)
print(diferencia_b)


#VERIFICACION CON LAS NOTAS DE CLASE

#Media móvil
# Datos
y <- c(18.43, 19.98, 19.51, 20.63,
       19.78, 21.25, 21.28, 22.14)

# Ajustar media móvil
resultado <- ajustar_mm(y, k = 3)

# Ver pronósticos
resultado$yhat

# Quitar los tres primeros valores
y_real <- y[4:length(y)]
y_pronostico <- resultado$yhat[4:length(y)]

# Calcular medidas de error
medidas(y_real, y_pronostico)

#Los resultados de los pronósticos coinciden con los obtenidos en las notas de clase. 
#En cuanto a las medidas de error, se presentan diferencias muy pequeñas: para el MSE se obtuvo 1,17 frente a 1,16 en las notas de clase; 
#para el MAD, 0,99 frente a 0,98; y para el MAPE, 4,7 % frente a 4,6 %. Estas diferencias son mínimas. 
#En general, los resultados obtenidos permiten verificar que la función de media móvil está funcionando correctamente.


#DOBLE MEDIA MÓVIL y HOLT

y <- c(133, 155, 165, 171, 194, 231,
       274, 312, 313, 333, 343)

# Doble media móvil k = 3

resultado_dmm <- ajustar_dmm(y, k = 3)

# Pronósticos extramuestrales
pronosticos_dmm <- resultado_dmm$pronosticar(3)

print(pronosticos_dmm)

# Medidas de error
posiciones <- !is.na(resultado_dmm$yhat)

error_dmm <- y[posiciones] - resultado_dmm$yhat[posiciones]

MSE_dmm <- mean(error_dmm^2)
MAD_dmm <- mean(abs(error_dmm))

cat("MSE DMM =", MSE_dmm, "\n")
cat("MAD DMM =", MAD_dmm, "\n")


# Holt lineal

resultado_holt <- ajustar_holt(
  y,
  alpha = 0.3,
  beta = 0.4
)

# Pronósticos extramuestrales
pronosticos_holt <- resultado_holt$pronosticar(3)

print(pronosticos_holt)

# Medidas de error
posiciones <- !is.na(resultado_holt$yhat)

error_holt <- y[posiciones] - resultado_holt$yhat[posiciones]

MSE_holt <- mean(error_holt^2)
MAD_holt <- mean(abs(error_holt))

cat("MSE Holt =", MSE_holt, "\n")
cat("MAD Holt =", MAD_holt, "\n")

#Los resultados obtenidos para la doble media móvil con k=3 y para el método de Holt lineal, utilizando alpha=0.3 y \beta=0.4, 
#coinciden con los resultados presentados en las notas de clase. 
#Tanto los pronósticos extramuestrales como las medidas de error MSE y MAD obtenidas para ambos métodos presentan los mismos valores, 
#lo que permite verificar que las funciones fueron implementadas correctamente.


#EJEMPLOS

library(knitr)
library(tidyverse)
library(knitr)
library(kableExtra)

# ANALISIS 16 SERIES

#AirPassengers
datos_air <- leer_serie(
  AirPassengers,
  fuente = "R",
  unidad = "pasajeros"
)

grafico_serie <- graficar_serie(
  datos_air,
  "Pasajeros aéreos"
)

resultado_cor <- correlograma(
  datos_air$y
)

panel <- panel_diagnostico(
  grafico_serie,
  resultado_cor
)

print(panel)

#Nile
datos_nile <- leer_serie(
  Nile,
  fuente = "R",
  unidad = "caudal"
)

grafico_serie <- graficar_serie(
  datos_nile,
  "Caudal del río Nilo"
)

cor_nile <- correlograma(
  datos_nile$y
)

panel <- panel_diagnostico(
  grafico_serie,
  cor_nile
)

print(panel)

# Airmiles
datos_airmiles <- leer_serie(
  airmiles,
  fuente = "R",
  unidad = "millas"
)

grafico_airmiles <- graficar_serie(
  datos_airmiles,
  "Millas de pasajeros"
)

cor_airmiles <- correlograma(
  datos_airmiles$y
)

panel_airmiles <- panel_diagnostico(
  grafico_airmiles,
  cor_airmiles
)

print(panel_airmiles)

# CO2
datos_co2 <- leer_serie(
  co2,
  fuente = "R",
  unidad = "ppm"
)

grafico_co2 <- graficar_serie(
  datos_co2,
  "Concentración de CO2"
)

cor_co2 <- correlograma(
  datos_co2$y
)

panel_co2 <- panel_diagnostico(
  grafico_co2,
  cor_co2
)

print(panel_co2)

#USAccDeath
datos_USA <- leer_serie(
  USAccDeaths,
  fuente = "R",
  unidad = "muertes"
)

grafico_USA <- graficar_serie(
  datos_USA,
  "Muertes por accidentes"
)

cor_USA <- correlograma(
  datos_USA$y
)

panel_USA <- panel_diagnostico(
  grafico_USA,
  cor_USA
)

print(panel_USA)

# Lynx
datos_lynx <- leer_serie(
  lynx,
  fuente = "R",
  unidad = "pieles"
)

grafico_lynx <- graficar_serie(
  datos_lynx,
  "Capturas de linces"
)

cor_lynx <- correlograma(
  datos_lynx$y
)

panel_lynx <- panel_diagnostico(
  grafico_lynx,
  cor_lynx
)

print(panel_lynx)

# LakeHuron
datos_Lake <- leer_serie(
  LakeHuron,
  fuente = "R",
  unidad = "pies"
)

grafico_Lake <- graficar_serie(
  datos_Lake,
  "Nivel del lago Huron"
)

cor_Lake <- correlograma(
  datos_Lake$y
)

panel_Lake <- panel_diagnostico(
  grafico_Lake,
  cor_Lake
)

print(panel_Lake)

#UKgas
datos_UK <- leer_serie(
  UKgas,
  fuente = "R",
  unidad = "millones de therms"
)

grafico_UK <- graficar_serie(
  datos_UK,
  "Consumo de gas"
)

cor_UK <- correlograma(
  datos_UK$y
)

panel_UK <- panel_diagnostico(
  grafico_UK,
  cor_UK
)

print(panel_UK)

#Sunspot.year
datos_sun <- leer_serie(
  sunspot.year,
  fuente = "R",
  unidad = "número de manchas solares"
)

grafico_sun <- graficar_serie(
  datos_sun,
  "Manchas solares"
)

cor_sun <- correlograma(
  datos_sun$y
)

panel_sun <- panel_diagnostico(
  grafico_sun,
  cor_sun
)

print(panel_sun)

# WWWusage
datos_W <- leer_serie(
  WWWusage,
  fuente = "R",
  unidad = "usuarios"
)

grafico_W <- graficar_serie(
  datos_W,
  "Uso de Internet"
)

cor_W <- correlograma(
  datos_W$y
)

panel_W <- panel_diagnostico(
  grafico_W,
  cor_W
)

print(panel_W)

# Austres
datos_austres <- leer_serie(
  austres,
  fuente = "R",
  unidad = "miles de personas"
)

grafico_austres <- graficar_serie(
  datos_austres,
  "Población australiana"
)

cor_austres <- correlograma(
  datos_austres$y
)

panel_austres <- panel_diagnostico(
  grafico_austres,
  cor_austres
)

print(panel_austres)

# JohnsonJohnson
datos_J <- leer_serie(
  JohnsonJohnson,
  fuente = "R",
  unidad = "millones de dólares"
)

grafico_J <- graficar_serie(
  datos_J,
  "Ganancias de Johnson & Johnson"
)

cor_J <- correlograma(
  datos_J$y
)

panel_J <- panel_diagnostico(
  grafico_J,
  cor_J
)

print(panel_J)

# UKDriverDeaths
datos_UKD <- leer_serie(
  UKDriverDeaths,
  fuente = "R",
  unidad = "muertes"
)

grafico_UKD <- graficar_serie(
  datos_UKD,
  "Muertes de conductores"
)

cor_UKD <- correlograma(
  datos_UKD$y
)

panel_UKD <- panel_diagnostico(
  grafico_UKD,
  cor_UKD
)

print(panel_UKD)

# Nottem
datos_nottem <- leer_serie(
  nottem,
  fuente = "R",
  unidad = "grados Fahrenheit"
)

grafico_nottem <- graficar_serie(
  datos_nottem,
  "Temperatura en Nottingham"
)

cor_nottem <- correlograma(
  datos_nottem$y
)

panel_nottem <- panel_diagnostico(
  grafico_nottem,
  cor_nottem
)

print(panel_nottem)

# Discoveries
datos_disc <- leer_serie(
  discoveries,
  fuente = "R",
  unidad = "descubrimientos"
)

grafico_disc <- graficar_serie(
  datos_disc,
  "Descubrimientos científicos"
)

cor_disc <- correlograma(
  datos_disc$y
)

panel_disc <- panel_diagnostico(
  grafico_disc,
  cor_disc
)

print(panel_disc)

# Seatbelts
datos_pet <- leer_serie(
  Seatbelts[, "PetrolPrice"],
  fuente = "R",
  unidad = "Precio"
)

grafico_pet <- graficar_serie(
  datos_pet,
  "Precio del Petróleo"
)

cor_pet <- correlograma(
  datos_pet$y
)

panel_pet <- panel_diagnostico(
  grafico_pet,
  cor_pet
)

print(panel_pet)

# Series Seleccionadas

# Tabla de decisión

tabla_decision <- data.frame(
  Serie     = c("Descubrimientos Científicos",
                "Temperatura en Nottingham",
                "Precio del Petróleo",
                "Población Australia", 
                "Millas de Psajeros",
                "Johnson & Johnson",
                "Uso de Internet"),
  Patrón    = c("Sin tendencia, Sin estacionalidad, No estacionaria", "Estacional, estacionaria, Sin tendencia", "Tendencia, Estacionalidad, No estacionaria", "Tendencia, No estacionalidad, No estacionaria", "Tendencia, No estacionalidad, No estacionaria", "Tendencia, Estacionalidad, No estacionaria", "Tendencia, No estacionalidad, No estacionaria"),
  Familia   = c("Media Simple/SES", "Media Móvil", "Doble Media Móvil", "Tendencia Lineal", "Tendencia Cuadrática", "Tendencia Exponencial", "Holt"),
  stringsAsFactors = FALSE
)

#Visualización
kable(tabla_decision,
      format    = "html",
      col.names = c("Serie", "Patrón", "Familia"),            # 3 columnas
      align     = c("l", "l", "l"),
      caption   = "Tabla de decisión — Diagnóstico de series de tiempo") |>
  kable_styling(bootstrap_options = "basic",
                full_width         = FALSE,
                position          = "center") |>
  row_spec(0, bold = TRUE) |>                                 # Encabezado en negrita
  row_spec(1:nrow(tabla_decision), extra_css = "border-bottom: none;") |> # Aplica a las 8 filas
  column_spec(1:3, border_left = FALSE, border_right = FALSE) |> # Sin líneas verticales (3 cols)
  add_header_above(c(" " = 3), line = TRUE, bold = TRUE) |>   # Línea superior APA (3 cols)
  row_spec(nrow(tabla_decision), extra_css = "border-bottom: 1px solid black;") # Línea inferior APA

# MEDIA SIMPLE - DESCUBRIMIENTOS CIENTÍFICOS

#Descripcion de la serie

#Crear dataframe con los resultados
propiedades_descubrimientos <- data.frame(
  Métrica    = c("Longitud de la serie (n)", 
                 "Año de inicio", 
                 "Año de fin", 
                 "Frecuencia observada"),
  Valor      = c(length(datos_disc$y),
                 start(discoveries)[1],
                 end(discoveries)[1],
                 frequency(discoveries)),
  Descripción = c("Número total de observaciones anuales",
                  "Primer año registrado en la serie",
                  "Último año registrado en la serie",
                  "Frecuencia anual de medición (1 dato/año)"),
  stringsAsFactors = FALSE
)

#Visualización de la tabla
kable(propiedades_descubrimientos,
      format    = "html",
      col.names = c("Métrica", "Valor", "Descripción"),
      align     = c("l", "c", "l"),
      caption   = "Propiedades temporales — Serie de Descubrimientos Científicos") |>
  kable_styling(bootstrap_options = "basic",
                full_width         = FALSE,
                position          = "center") |>
  row_spec(0, bold = TRUE) |>                                               # Encabezado en negrita
  row_spec(1:nrow(propiedades_descubrimientos), extra_css = "border-bottom: none;") |> # Sin líneas intermedias
  column_spec(1:3, border_left = FALSE, border_right = FALSE) |>             # Sin líneas verticales
  add_header_above(c(" " = 3), line = TRUE, bold = TRUE) |>                 # Línea superior APA
  row_spec(nrow(propiedades_descubrimientos), extra_css = "border-bottom: 1px solid black;") # Línea inferior APA

#Grafica de la serie, ACF Y PACF
datos_disc <- leer_serie(
  discoveries,
  fuente = "R",
  unidad = "descubrimientos"
)

grafico_disc <- graficar_serie(
  datos_disc,
  "Descubrimientos científicos"
)

cor_disc <- correlograma(
  datos_disc$y
)

panel_disc <- panel_diagnostico(
  grafico_disc,
  cor_disc
)

print(panel_disc)

#Contraste individual de los $r_h$

#Contraste Individual
r <- cor_disc$acf

T <- length(datos_disc$y)

z <- r * sqrt(T)

valor_p <- 2 * pnorm(-abs(z))

valor_critico <- qnorm(0.975)

tabla_acf <- data.frame(
  rezago = 1:length(r),
  r_h = r,
  estadistico = z,
  valor_p = valor_p,
  significativo = abs(z) > valor_critico
)

# Ajustar el data frame para mejorar la presentación numérica
tabla_acf_formato <- tabla_acf
tabla_acf_formato$r_h         <- round(tabla_acf_formato$r_h, 4)
tabla_acf_formato$estadistico <- round(tabla_acf_formato$estadistico, 4)
tabla_acf_formato$valor_p     <- format.pval(tabla_acf_formato$valor_p, digits = 4, eps = 0.0001)
tabla_acf_formato$significativo <- ifelse(tabla_acf_formato$significativo, "Sí", "No")

#Visualización estilo APA
kable(tabla_acf_formato,
      format    = "html",
      col.names = c("Rezago ($h$)", "Autocorrelación ($r_h$)", "Estadístico ($z$)", "Valor $p$", "Significativo"),
      align     = c("c", "c", "c", "c", "c"),
      caption   = "Contraste individual de autocorrelación — Serie de Descubrimientos") |>
  kable_styling(bootstrap_options = "basic",
                full_width         = FALSE,
                position          = "center") |>
  row_spec(0, bold = TRUE) |>                                             # Encabezado en negrita
  row_spec(1:nrow(tabla_acf_formato), extra_css = "border-bottom: none;") |> # Sin líneas intermedias
  column_spec(1:5, border_left = FALSE, border_right = FALSE) |>           # Sin líneas verticales
  add_header_above(c(" " = 5), line = TRUE, bold = TRUE) |>               # Línea superior APA
  row_spec(nrow(tabla_acf_formato), extra_css = "border-bottom: 1px solid black;")

#Ljung Box
resultado_lb_disc <- ljung_box(
  r = cor_disc$acf,
  T = length(datos_disc$y),
  m = 24,
  p = 0
)

cat(
  "Descubrimientos → Q =", round(resultado_lb_disc$estadistico, 4),
  "| gl =", resultado_lb_disc$grados_libertad,
  "| valor crítico =", round(resultado_lb_disc$valor_critico, 4),
  "| p-valor =", round(resultado_lb_disc$valor_p, 4), "\n"
)

#Partición de la serie
h <- min(
  12,
  floor(0.2 * T)
)

y_estimacion <- datos_disc$y[1:(T - h)]

y_validacion <- datos_disc$y[(T - h + 1):T]

cat(
  "Descubrimientos → Estimación =", length(y_estimacion),
  "| Validación =", length(y_validacion), "\n"
)

#Ajustar Media Simple
modelo_disc <- ajustar_media(
  y_estimacion
)

#Pronósticos
pronosticos_disc <- modelo_disc$pronosticar(h)
cat(
  "Horizonte de validación (h):", h, "\n",
  "Pronóstico constante:", round(pronosticos_disc[1], 4), "\n"
)

#Medidas de error
posiciones <- which(
  !is.na(modelo_disc$yhat)
)

medidas_estimacion <- medidas(
  y_estimacion[posiciones],
  modelo_disc$yhat[posiciones]
)

medidas_validacion <- medidas(
  y_validacion,
  pronosticos_disc
)

#Tabla medidas de error
tabla_medidas_disc <- data.frame(
  Tramo = c("Estimación", "Validación"),
  MSE = c(
    round(medidas_estimacion$MSE, 4),
    round(medidas_validacion$MSE, 4)
  ),
  MAD = c(
    round(medidas_estimacion$MAD, 4),
    round(medidas_validacion$MAD, 4)
  ),
  MAPE = c(
    medidas_estimacion$MAPE,
    medidas_validacion$MAPE
  ),
  MASE = c(
    round(medidas_estimacion$MASE, 4),
    round(medidas_validacion$MASE, 4)
  )
)

tabla_medidas_disc

#Referente ingenuo
pronosticos_ingenuo <- rep(
  y_estimacion[length(y_estimacion)],
  h
)

medidas_ingenuo <- medidas(
  y_validacion,
  pronosticos_ingenuo
)
cat(
  "Horizonte de validación (h):", h, "\n",
  "Pronóstico ingenuo:", pronosticos_ingenuo[1], "\n"
)

#Tabla medidas de error del ingenuo
tabla_ingenuo <- data.frame(
  Metodo = "Ingenuo",
  MSE = round(medidas_ingenuo$MSE, 4),
  MAD = round(medidas_ingenuo$MAD, 4),
  MAPE = medidas_ingenuo$MAPE,
  MASE = round(medidas_ingenuo$MASE, 4)
)

tabla_ingenuo

#Escala MASE 
error_ingenuo_estimacion <- abs(
  y_estimacion[2:length(y_estimacion)] -
    y_estimacion[1:(length(y_estimacion) - 1)]
)

escala_mase <- mean(error_ingenuo_estimacion)

cat(
  "Escala MASE (ingenuo en estimación):",
  round(escala_mase, 4),
  "\n"
)

# MASE corregido usando la escala del ingenuo en estimación

MASE_media_simple <- medidas_validacion$MAD / escala_mase

MASE_ingenuo <- medidas_ingenuo$MAD / escala_mase

cat(
  "MASE media simple:", round(MASE_media_simple, 4), "\n",
  "MASE ingenuo:", round(MASE_ingenuo, 4), "\n"
)

#Tabla de comparacion
tabla_comparacion <- data.frame(
  Metodo = c("Media simple", "Ingenuo"),
  MSE = c(
    round(medidas_validacion$MSE, 4),
    round(medidas_ingenuo$MSE, 4)
  ),
  MAD = c(
    round(medidas_validacion$MAD, 4),
    round(medidas_ingenuo$MAD, 4)
  ),
  MAPE = c(
    medidas_validacion$MAPE,
    medidas_ingenuo$MAPE
  ),
  MASE = c(
    round(MASE_media_simple, 4),
    round(MASE_ingenuo, 4)
  )
)
tabla_comparacion

#Errores de pronóstico dentro de la estimación
posiciones <- which(!is.na(modelo_disc$yhat))
errores_disc <- y_estimacion[posiciones] - modelo_disc$yhat[posiciones]

#Validación de los errores
resultado_errores_disc <- validar_errores(
  e = errores_disc,
  m = NULL,
  p = 0
)

#Grafico de los errores
resultado_errores_disc$grafico_errores

#Correlograma de los errores
resultado_errores_disc$correlograma$grafico

#Prueba t erores
cat(
  "Estadístico de prueba:", round(resultado_errores_disc$t, 4), "\n",
  "Grados de libertad:", resultado_errores_disc$grados_libertad_t, "\n",
  "Valor crítico:", round(
    qt(0.975, resultado_errores_disc$grados_libertad_t), 4
  ), "\n",
  "Valor p:", round(resultado_errores_disc$valor_p_t, 4), "\n"
)

#Lung Box errores
cat(
  "Estadístico de prueba:", round(
    resultado_errores_disc$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_disc$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_disc$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    resultado_errores_disc$ljung_box$valor_p, 4
  ), "\n"
)

#Jarque Bera errores
cat(
  "Estadístico de prueba:", round(
    resultado_errores_disc$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_disc$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_disc$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    resultado_errores_disc$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

#Durbin Watson errores
cat(
  "Estadístico de prueba:", round(
    resultado_errores_disc$durbin_watson$estadistico, 4
  ), "\n",
  "Grados de libertad: no aplica\n",
  "Valor crítico: no calculado\n",
  "Valor p: no calculado\n"
)

#Grafico final
ggplot2::ggplot(
  data.frame(
    tiempo = 1:T,
    real = datos_disc$y,
    ajuste = c(modelo_disc$yhat, rep(NA, h)),
    pronostico = c(rep(NA, T - h), pronosticos_disc)
  ),
  ggplot2::aes(x = tiempo)
) +
  
  # Serie real
  ggplot2::geom_line(
    ggplot2::aes(y = real),
    color = "purple"
  ) +
  
  # Ajuste de media simple
  ggplot2::geom_line(
    ggplot2::aes(y = ajuste),
    color = "green"
  ) +
  
  # Pronóstico
  ggplot2::geom_line(
    ggplot2::aes(y = pronostico),
    color = "green"
  ) +
  
  # Separación entre estimación y validación
  ggplot2::geom_vline(
    xintercept = T - h,
    color = "red",
    linetype = "dashed"
  ) +
  
  ggplot2::labs(
    title = "Discoveries: ajuste y pronóstico de media simple",
    x = "Tiempo",
    y = "Número de descubrimientos"
  ) +
  
  ggplot2::theme_minimal()


# MEDIA MOVIL - TEMPERATURA NOTTINGHAM

#Descripción de la serie
# Crear dataframe con los resultados
propiedades_nottem <- data.frame(
  Métrica = c(
    "Longitud de la serie (n)",
    "Fecha de inicio",
    "Fecha de fin",
    "Frecuencia observada"
  ),
  
  Valor = c(
    length(datos_nottem$y),
    paste0(start(nottem)[1], "-", sprintf("%02d", start(nottem)[2])),
    paste0(end(nottem)[1], "-", sprintf("%02d", end(nottem)[2])),
    frequency(nottem)
  ),
  
  Descripción = c(
    "Número total de observaciones mensuales",
    "Primer mes registrado en la serie",
    "Último mes registrado en la serie",
    "Frecuencia mensual de medición (12 datos/año)"
  ),
  
  stringsAsFactors = FALSE
)

# Visualización de la tabla
kable(
  propiedades_nottem,
  format = "html",
  col.names = c("Métrica", "Valor", "Descripción"),
  align = c("l", "c", "l"),
  caption = "Propiedades temporales — Serie de Temperatura de Nottingham"
) |>
  kable_styling(
    bootstrap_options = "basic",
    full_width = FALSE,
    position = "center"
  ) |>
  row_spec(0, bold = TRUE) |>
  row_spec(
    1:nrow(propiedades_nottem),
    extra_css = "border-bottom: none;"
  ) |>
  column_spec(
    1:3,
    border_left = FALSE,
    border_right = FALSE
  ) |>
  add_header_above(
    c(" " = 3),
    line = TRUE,
    bold = TRUE
  ) |>
  row_spec(
    nrow(propiedades_nottem),
    extra_css = "border-bottom: 1px solid black;"
  )

#SERIE, ACF Y PACF
datos_nottem <- leer_serie(
  nottem,
  fuente = "R",
  unidad = "grados Fahrenheit"
)

grafico_nottem <- graficar_serie(
  datos_nottem,
  "Temperatura en Nottingham"
)

cor_nottem <- correlograma(
  datos_nottem$y
)

panel_nottem <- panel_diagnostico(
  grafico_nottem,
  cor_nottem
)
print(panel_nottem)

#Contraste individual
r <- cor_nottem$acf
T <- length(datos_nottem$y)
z <- r * sqrt(T)

valor_p <- 2 * pnorm(-abs(z))

valor_critico <- qnorm(0.975)

tabla_acf_nottem <- data.frame(
  rezago = 1:length(r),
  r_h = r,
  estadistico = z,
  valor_p = valor_p,
  significativo = abs(z) > valor_critico
)

# Ajustar el data frame para mejorar la presentación numérica
tabla_acf_nottem_formato <- tabla_acf_nottem

tabla_acf_nottem_formato$r_h <-
  round(tabla_acf_nottem_formato$r_h, 4)

tabla_acf_nottem_formato$estadistico <-
  round(tabla_acf_nottem_formato$estadistico, 4)

tabla_acf_nottem_formato$valor_p <-
  format.pval(
    tabla_acf_nottem_formato$valor_p,
    digits = 4,
    eps = 0.0001
  )

tabla_acf_nottem_formato$significativo <-
  ifelse(
    tabla_acf_nottem_formato$significativo,
    "Sí",
    "No"
  )

# Visualización estilo APA
kable(
  tabla_acf_nottem_formato,
  format = "html",
  col.names = c(
    "Rezago ($h$)",
    "Autocorrelación ($r_h$)",
    "Estadístico ($z$)",
    "Valor $p$",
    "Significativo"
  ),
  align = c("c", "c", "c", "c", "c"),
  caption = "Contraste individual de autocorrelación — Serie de Temperatura de Nottingham"
) |>
  kable_styling(
    bootstrap_options = "basic",
    full_width = FALSE,
    position = "center"
  ) |>
  row_spec(0, bold = TRUE) |>
  row_spec(
    1:nrow(tabla_acf_nottem_formato),
    extra_css = "border-bottom: none;"
  ) |>
  column_spec(
    1:5,
    border_left = FALSE,
    border_right = FALSE
  ) |>
  add_header_above(
    c(" " = 5),
    line = TRUE,
    bold = TRUE
  ) |>
  row_spec(
    nrow(tabla_acf_nottem_formato),
    extra_css = "border-bottom: 1px solid black;"
  )

#Lung Box
resultado_lb_nottem <- ljung_box(
  r = cor_nottem$acf,
  T = length(datos_nottem$y),
  m = 24,
  p = 0
)
cat(
  "Nottingham → Q =", round(resultado_lb_nottem$estadistico, 4),
  "| gl =", resultado_lb_nottem$grados_libertad,
  "| valor crítico =", round(resultado_lb_nottem$valor_critico, 4),
  "| p-valor =", round(resultado_lb_nottem$valor_p, 4), "\n"
)

#Particion de la serie
# Número total de observaciones
T_nottem <- length(datos_nottem$y)

# Horizonte de validación
h_nottem <- min(
  12,
  floor(0.2 * T_nottem)
)

# Datos de estimación
y_estimacion_nt <- datos_nottem$y[1:(T_nottem - h_nottem)]

# Datos de validación
y_validacion_nt <- datos_nottem$y[
  (T_nottem - h_nottem + 1):T_nottem
]

# Optimización de la ventana de la media móvil
resultado_opt_nottem <- optimizar(
  y = y_estimacion_nt,
  metodo = "mm",
  rejilla = 2:12
)
resultado_opt_nottem$rejilla

#Gráfica de optimización
grafico_opt_nottem <- graficar_optimizacion(
  resultado = resultado_opt_nottem,
  parametro = "k",
  titulo = "Optimización de la ventana de media móvil - Nottingham"
)
grafico_opt_nottem

# Ajuste de la media móvil con el k óptimo
modelo_nottem <- ajustar_mm(
  y = y_estimacion_nt,
  k = resultado_opt_nottem$optimo$k
)

# Pronóstico de los 12 meses de validación
pronosticos_nottem <- modelo_nottem$pronosticar(h_nottem)
cat(
  "Horizonte de validación (h):", h_nottem, "\n",
  "Pronóstico constante:", round(pronosticos_nottem[1], 4), "\n"
)

#Medidas de error
posiciones_nottem <- which(!is.na(modelo_nottem$yhat))

sum(is.na(y_estimacion_nt[posiciones_nottem]))

# Medidas de estimación
medidas_estimacion_nottem <- medidas(
  y = y_estimacion_nt[posiciones_nottem],
  yhat = modelo_nottem$yhat[posiciones_nottem]
)

# Medidas de validación
medidas_validacion_nottem <- medidas(
  y = y_validacion_nt,
  yhat = pronosticos_nottem
)

# Pronóstico ingenuo estacional
pronosticos_ingenuo_estacional_nottem <- 
  y_estimacion_nt[(length(y_estimacion_nt) - 12 + 1):length(y_estimacion_nt)]

# Medidas del ingenuo estacional en validación
medidas_ingenuo_estacional_nottem <- medidas(
  y = y_validacion_nt,
  yhat = pronosticos_ingenuo_estacional_nottem
)

# Escala MASE usando el ingenuo estacional en estimación
escala_mase_nottem <- mean(
  abs(
    y_estimacion_nt[13:length(y_estimacion_nt)] -
      y_estimacion_nt[1:(length(y_estimacion_nt) - 12)]
  )
)

#Mase de la media móvil
mase_mm_nottem <- 
  medidas_validacion_nottem$MAD / escala_mase_nottem

mase_ingenuo_estacional_nottem <- 
  medidas_ingenuo_estacional_nottem$MAD / escala_mase_nottem

mase_mm_estim_nottem <- medidas_estimacion_nottem$MAD / escala_mase_nottem

#Medidas
tabla_medidas_nottem <- data.frame(
  Tramo = c(
    "Estimación",
    "Validación",
    "Validación"
  ),
  Método = c(
    "Media móvil",
    "Media móvil",
    "Ingenuo estacional"
  ),
  MSE = c(
    medidas_estimacion_nottem$MSE,
    medidas_validacion_nottem$MSE,
    medidas_ingenuo_estacional_nottem$MSE
  ),
  MAD = c(
    medidas_estimacion_nottem$MAD,
    medidas_validacion_nottem$MAD,
    medidas_ingenuo_estacional_nottem$MAD
  ),
  MAPE = c(
    medidas_estimacion_nottem$MAPE,
    medidas_validacion_nottem$MAPE,
    medidas_ingenuo_estacional_nottem$MAPE
  ),
  MASE = c(
    mase_mm_estim_nottem,
    mase_mm_nottem,
    mase_ingenuo_estacional_nottem
  )
)

tabla_medidas_nottem

# Errores de la media móvil en estimación
posiciones_nottem <- which(!is.na(modelo_nottem$yhat))

errores_nottem <- 
  y_estimacion_nt[posiciones_nottem] -
  modelo_nottem$yhat[posiciones_nottem]

resultado_errores_nottem <- validar_errores(
  e = errores_nottem,
  m = NULL,
  p = 1
)

#Grafico errores
resultado_errores_nottem$grafico_errores

#ACF y PACF errores
resultado_errores_nottem$correlograma$grafico

#Prueba t errores
cat(
  "Estadístico de prueba:", round(resultado_errores_nottem$t, 4), "\n",
  "Grados de libertad:", resultado_errores_nottem$grados_libertad_t, "\n",
  "Valor crítico:", round(
    qt(0.975, resultado_errores_nottem$grados_libertad_t), 4
  ), "\n",
  "Valor p:", round(resultado_errores_nottem$valor_p_t, 4), "\n"
)

#Ljung Box errores
cat(
  "Estadístico de prueba:", round(
    resultado_errores_nottem$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_nottem$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_nottem$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    resultado_errores_nottem$ljung_box$valor_p, 4
  ), "\n"
)

#Jarque Bera errores
cat(
  "Estadístico de prueba:", round(
    resultado_errores_nottem$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_nottem$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_nottem$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    resultado_errores_nottem$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

#Durbin Watson
cat(
  "Estadístico de prueba:", round(
    resultado_errores_nottem$durbin_watson$estadistico, 4
  ), "\n",
  "Grados de libertad: no aplica\n",
  "Valor crítico: no calculado\n",
  "Valor p: no calculado\n"
)

#Grafico final
ggplot2::ggplot(
  data.frame(
    tiempo = 1:T,
    real = datos_nottem$y,
    ajuste = c(modelo_nottem$yhat, rep(NA, h)),
    pronostico = c(rep(NA, T - h), pronosticos_nottem)
  ),
  ggplot2::aes(x = tiempo)
) +
  
  # Serie real
  ggplot2::geom_line(
    ggplot2::aes(y = real),
    color = "purple"
  ) +
  
  # Ajuste de media simple
  ggplot2::geom_line(
    ggplot2::aes(y = ajuste),
    color = "green"
  ) +
  
  # Pronóstico
  ggplot2::geom_line(
    ggplot2::aes(y = pronostico),
    color = "green"
  ) +
  
  # Separación entre estimación y validación
  ggplot2::geom_vline(
    xintercept = T - h,
    color = "red",
    linetype = "dashed"
  ) +
  
  ggplot2::labs(
    title = "Temperatura Nottingham: ajuste y pronóstico de media móvil",
    x = "Tiempo",
    y = "Temperatura (°F)"
  ) +
  
  ggplot2::theme_minimal()


# SES - Descubrimientos cientificos

# Se reutiliza la misma partición del ejemplo de media simple
T_disc <- length(datos_disc$y)

h_disc_ses <- h  

y_estimacion_disc_ses <- y_estimacion
y_validacion_disc_ses <- y_validacion

cat(
  "Descubrimientos (SES) → Estimación =", length(y_estimacion_disc_ses),
  "| Validación =", length(y_validacion_disc_ses), "\n"
)

#Optimizacion de alpha
resultado_opt_disc_ses <- optimizar(
  y = y_estimacion_disc_ses,
  metodo = "ses",
  rejilla = seq(0.1, 0.9, by = 0.1)
)

resultado_opt_disc_ses$rejilla

#Grafico de optimizacion
grafico_opt_disc_ses <- graficar_optimizacion(
  resultado = resultado_opt_disc_ses,
  parametro = "alpha",
  titulo = "Optimización de alpha - Descubrimientos científicos (SES)"
)

grafico_opt_disc_ses

#Ajustar ses
modelo_disc_ses <- ajustar_ses(
  y = y_estimacion_disc_ses,
  alpha = resultado_opt_disc_ses$optimo$alpha
)

pronosticos_disc_ses <- modelo_disc_ses$pronosticar(h_disc_ses)

cat(
  "Horizonte de validación (h):", h_disc_ses, "\n",
  "Alpha óptimo:", resultado_opt_disc_ses$optimo$alpha, "\n",
  "Pronóstico constante:", round(pronosticos_disc_ses[1], 4), "\n"
)

#Medidas de error
#posiciones
posiciones_disc_ses <- which(!is.na(modelo_disc_ses$yhat))

medidas_estimacion_disc_ses <- medidas(
  y = y_estimacion_disc_ses[posiciones_disc_ses],
  yhat = modelo_disc_ses$yhat[posiciones_disc_ses]
)

medidas_validacion_disc_ses <- medidas(
  y = y_validacion_disc_ses,
  yhat = pronosticos_disc_ses
)

#Rferente ingenuo
pronosticos_ingenuo_disc_ses <- rep(
  y_estimacion_disc_ses[length(y_estimacion_disc_ses)],
  h_disc_ses
)

medidas_ingenuo_disc_ses <- medidas(
  y = y_validacion_disc_ses,
  yhat = pronosticos_ingenuo_disc_ses
)

#Escala del MASE
escala_mase_disc_ses <- mean(
  abs(
    y_estimacion_disc_ses[2:length(y_estimacion_disc_ses)] -
      y_estimacion_disc_ses[1:(length(y_estimacion_disc_ses) - 1)]
  )
)

mase_estim_disc_ses <- medidas_estimacion_disc_ses$MAD / escala_mase_disc_ses
mase_valid_disc_ses <- medidas_validacion_disc_ses$MAD / escala_mase_disc_ses
mase_ingenuo_disc_ses <- medidas_ingenuo_disc_ses$MAD / escala_mase_disc_ses

#Tabla de medidas
tabla_medidas_disc_ses <- data.frame(
  Tramo = c("Estimación", "Validación", "Validación"),
  Método = c("SES", "SES", "Ingenuo"),
  MSE = c(
    medidas_estimacion_disc_ses$MSE,
    medidas_validacion_disc_ses$MSE,
    medidas_ingenuo_disc_ses$MSE
  ),
  MAD = c(
    medidas_estimacion_disc_ses$MAD,
    medidas_validacion_disc_ses$MAD,
    medidas_ingenuo_disc_ses$MAD
  ),
  MAPE = c(
    medidas_estimacion_disc_ses$MAPE,
    medidas_validacion_disc_ses$MAPE,
    medidas_ingenuo_disc_ses$MAPE
  ),
  MASE = c(
    mase_estim_disc_ses,
    mase_valid_disc_ses,
    mase_ingenuo_disc_ses
  )
)

tabla_medidas_disc_ses

#Validacion de errores
posiciones_disc_ses <- which(!is.na(modelo_disc_ses$yhat))

errores_disc_ses <- y_estimacion_disc_ses[posiciones_disc_ses] - modelo_disc_ses$yhat[posiciones_disc_ses]

resultado_errores_disc_ses <- validar_errores(
  e = errores_disc_ses,
  m = NULL,
  p = 1    # SES: 1 parámetro (alpha)
)

#Grafico de errores
resultado_errores_disc_ses$grafico_errores

#ACF Y PACF errores
resultado_errores_disc_ses$correlograma$grafico

#Prueba t errores
cat(
  "Estadístico de prueba:", round(resultado_errores_disc_ses$t, 4), "\n",
  "Grados de libertad:", resultado_errores_disc_ses$grados_libertad_t, "\n",
  "Valor crítico:", round(
    qt(0.975, resultado_errores_disc_ses$grados_libertad_t), 4
  ), "\n",
  "Valor p:", round(resultado_errores_disc_ses$valor_p_t, 4), "\n"
)

#Ljung Box errores
cat(
  "Estadístico de prueba:", round(
    resultado_errores_disc_ses$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_disc_ses$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_disc_ses$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    resultado_errores_disc_ses$ljung_box$valor_p, 4
  ), "\n"
)

#Jarque Bera errores
cat(
  "Estadístico de prueba:", round(
    resultado_errores_disc_ses$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_disc_ses$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_disc_ses$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    resultado_errores_disc_ses$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

#Durbin Watson
cat(
  "Estadístico de prueba:", round(
    resultado_errores_disc_ses$durbin_watson$estadistico, 4
  ), "\n",
  "Grados de libertad: no aplica\n",
  "Valor crítico: no calculado\n",
  "Valor p: no calculado\n"
)

#Grafico final
ggplot2::ggplot(
  data.frame(
    tiempo = 1:T_disc,
    real = datos_disc$y,
    ajuste = c(modelo_disc_ses$yhat, rep(NA, h_disc_ses)),
    pronostico = c(rep(NA, T_disc - h_disc_ses), pronosticos_disc_ses)
  ),
  ggplot2::aes(x = tiempo)
) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = real),
    color = "purple"
  ) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = ajuste),
    color = "green"
  ) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = pronostico),
    color = "green"
  ) +
  
  ggplot2::geom_vline(
    xintercept = T_disc - h_disc_ses,
    color = "red",
    linetype = "dashed"
  ) +
  
  ggplot2::labs(
    title = "Descubrimientos científicos: ajuste y pronóstico de SES",
    x = "Tiempo",
    y = "Número de descubrimientos"
  ) +
  
  ggplot2::theme_minimal()


# DOBLE MEDIA MOVIL - PRECIOS DEL PETROLEO

# Descripción de la serie

# Crear dataframe con los resultados
propiedades_pet <- data.frame(
  Métrica = c(
    "Longitud de la serie (n)",
    "Año de inicio",
    "Año de fin",
    "Frecuencia observada"
  ),
  
  Valor = c(
    length(datos_pet$y),
    start(Seatbelts[, "PetrolPrice"])[1],
    end(Seatbelts[, "PetrolPrice"])[1],
    frequency(Seatbelts[, "PetrolPrice"])
  ),
  
  Descripción = c(
    "Número total de observaciones anuales",
    "Primer año registrado en la serie",
    "Último año registrado en la serie",
    "Frecuencia anual de medición (1 dato/año)"
  ),
  
  stringsAsFactors = FALSE
)

# Visualización de la tabla
kable(
  propiedades_pet,
  format = "html",
  col.names = c("Métrica", "Valor", "Descripción"),
  align = c("l", "c", "l"),
  caption = "Propiedades temporales — Serie CO2"
) |>
  kable_styling(
    bootstrap_options = "basic",
    full_width = FALSE,
    position = "center"
  ) |>
  row_spec(0, bold = TRUE) |> 
  row_spec(
    1:nrow(propiedades_pet),
    extra_css = "border-bottom: none;"
  ) |>
  column_spec(
    1:3,
    border_left = FALSE,
    border_right = FALSE
  ) |>
  add_header_above(
    c(" " = 3),
    line = TRUE,
    bold = TRUE
  ) |>
  row_spec(
    nrow(propiedades_pet),
    extra_css = "border-bottom: 1px solid black;"
  )

#Serie, ACF y PACF
datos_pet <- leer_serie(
  Seatbelts[, "PetrolPrice"],
  fuente = "R",
  unidad = "caudal"
)

grafico_serie <- graficar_serie(
  datos_pet,
  "Precio del Petróleo"
)

cor_pet <- correlograma(
  datos_pet$y
)

panel <- panel_diagnostico(
  grafico_serie,
  cor_pet
)

print(panel)

# Contrastes individuales
r <- cor_pet$acf

# Número de observaciones
T <- length(datos_pet$y)

# Estadístico de prueba
z <- r * sqrt(T)

# Valor p bilateral
valor_p <- 2 * pnorm(-abs(z))

# Valor crítico al 5 %
valor_critico <- qnorm(0.975)

# Crear tabla de resultados
tabla_acf_pet <- data.frame(
  rezago = 1:length(r),
  r_h = r,
  estadistico = z,
  valor_p = valor_p,
  significativo = abs(z) > valor_critico
)
tabla_acf_pet

#Ljung Box
resultado_lb_pet <- ljung_box(
  r = cor_pet$acf,
  T = length(datos_pet$y),
  m = 24,
  p = 0
)
cat(
  "Precio del Petróleo → Q =", round(resultado_lb_pet$estadistico, 4),
  "| gl =", resultado_lb_pet$grados_libertad,
  "| valor crítico =", round(resultado_lb_pet$valor_critico, 4),
  "| p-valor =", round(resultado_lb_pet$valor_p, 4), "\n"
)

# Partición de la serie
T_pet <- length(datos_pet$y)

h_pet <- min(
  12,
  floor(0.2 * T_pet)
)

y_estimacion_pet <- datos_pet$y[1:(T_pet - h_pet)]

y_validacion_pet <- datos_pet$y[
  (T_pet - h_pet + 1):T_pet
]
cat(
  "Petróleo → Estimación =", length(y_estimacion_pet),
  "| Validación =", length(y_validacion_pet), "\n"
)

# Buscar el mejor valor de k
resultado_opt_pet <- optimizar(
  y = y_estimacion_pet,
  metodo = "dmm",
  rejilla = 2:12
)
resultado_opt_pet$rejilla

#Gráfica de optimización
grafico_opt_pet <- graficar_optimizacion(
  resultado = resultado_opt_pet,
  parametro = "k",
  titulo = "Optimización de la ventana de media móvil - Muerte de Conductores"
)
grafico_opt_pet

#Ajustar doble media móvil
modelo_pet <- ajustar_dmm(
  y = y_estimacion_pet,
  k = resultado_opt_pet$optimo$k
)
pronosticos_pet <- modelo_pet$pronosticar(h)

cat(
  "Horizonte de validación (h):", h, "\n",
  "Pronóstico:", round(pronosticos_pet[1:4], 4), "\n"
)

# Medidas de error en el período de validación
medidas_pet <- medidas(
  y = y_validacion_pet,
  yhat = pronosticos_pet
)

# Pronósticos del método ingenuo
pronosticos_ingenuo_pet <- rep(
  y_estimacion_pet[length(y_estimacion_pet)],
  h
)

# Medidas de error del ingenuo
medidas_ingenuo_pet <- medidas(
  y = y_validacion_pet,
  yhat = pronosticos_ingenuo_pet
)

# Escala del MASE usando el ingenuo en estimación
escala_mase_pet <- mean(
  abs(
    y_estimacion_pet[2:length(y_estimacion_pet)] -
      y_estimacion_pet[1:(length(y_estimacion_pet) - 1)]
  )
)

# MASE correcto de cada método
mase_media_pet <- medidas_pet$MAD / escala_mase_pet
mase_ingenuo_pet <- medidas_ingenuo_pet$MAD / escala_mase_pet

# Tabla de comparación
comparacion_pet <- data.frame(
  Método = c("Doble Media móvil", "Ingenuo"),
  MSE = c(medidas_pet$MSE, medidas_ingenuo_pet$MSE),
  MAD = c(medidas_pet$MAD, medidas_ingenuo_pet$MAD),
  MAPE = c(medidas_pet$MAPE, medidas_ingenuo_pet$MAPE),
  MASE = c(mase_media_pet, mase_ingenuo_pet)
)

comparacion_pet

# Validación de los errores

# Posiciones donde existe pronóstico
posiciones_pet <- which(!is.na(modelo_pet$yhat))

# Errores de un paso
errores_pet <- 
  y_estimacion_pet[posiciones_pet] -
  modelo_pet$yhat[posiciones_pet]

resultado_errores_pet <- validar_errores(
  e = errores_pet,
  m = NULL,
  p = 1
)

#Grafico de los errores
resultado_errores_pet$grafico_errores

#ACF, PACF errores
resultado_errores_pet$correlograma$grafico

#Prueba t errores
cat(
  "Estadístico de prueba:", round(resultado_errores_pet$t, 4), "\n",
  "Grados de libertad:", resultado_errores_pet$grados_libertad_t, "\n",
  "Valor crítico:", round(
    qt(0.975, resultado_errores_pet$grados_libertad_t), 4
  ), "\n",
  "Valor p:", round(resultado_errores_pet$valor_p_t, 4), "\n"
)

#Ljung Box
cat(
  "Estadístico de prueba:", round(
    resultado_errores_pet$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_pet$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_pet$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    resultado_errores_pet$ljung_box$valor_p, 4
  ), "\n"
)

#Jarque Bera
cat(
  "Estadístico de prueba:", round(
    resultado_errores_pet$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_pet$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_pet$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    resultado_errores_pet$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

#Durbin Watson
cat(
  "Estadístico de prueba:", round(
    resultado_errores_pet$durbin_watson$estadistico, 4
  ), "\n",
  "Grados de libertad: no aplica\n",
  "Valor crítico: no calculado\n",
  "Valor p: no calculado\n"
)

#Grafico final
ggplot2::ggplot(
  data.frame(
    tiempo = 1:T_pet,
    real = datos_pet$y,
    ajuste = c(modelo_pet$yhat, rep(NA, h_pet)),
    pronostico = c(rep(NA, T_pet - h_pet), pronosticos_pet)
  ),
  ggplot2::aes(x = tiempo)
) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = real),
    color = "purple"
  ) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = ajuste),
    color = "green"
  ) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = pronostico),
    color = "green"
  ) +
  
  ggplot2::geom_vline(
    xintercept = T_pet - h_pet,
    color = "red",
    linetype = "dashed"
  ) +
  
  ggplot2::labs(
    title = "[Precio del Petróleo: ajuste y pronóstico de DMM",
    x = "Tiempo",
    y = "Precio"
  ) +
  
  ggplot2::theme_minimal()


# TENDENCIA LINEAL - POBLACION AUSTRALIANA

# Descripción de la serie
#Crear dataframe con los resultados
propiedades_austres <- data.frame(
  Métrica    = c("Longitud de la serie (n)", 
                 "Año de inicio", 
                 "Año de fin", 
                 "Frecuencia observada"),
  Valor      = c(length(datos_austres$y),
                 start(austres)[1],
                 end(austres)[1],
                 frequency(austres)),
  Descripción = c("Número total de observaciones trimestrales",
                  "Primer año registrado en la serie",
                  "Último año registrado en la serie",
                  "Frecuencia trimestral de medición (4 datos/año)"),
  stringsAsFactors = FALSE
)

#Visualización de la tabla
kable(propiedades_austres,
      format    = "html",
      col.names = c("Métrica", "Valor", "Descripción"),
      align     = c("l", "c", "l"),
      caption   = "Propiedades temporales — Serie de Población Australiana") |>
  kable_styling(bootstrap_options = "basic",
                full_width         = FALSE,
                position          = "center") |>
  row_spec(0, bold = TRUE) |>
  row_spec(1:nrow(propiedades_austres), extra_css = "border-bottom: none;") |>
  column_spec(1:3, border_left = FALSE, border_right = FALSE) |>
  add_header_above(c(" " = 3), line = TRUE, bold = TRUE) |>
  row_spec(nrow(propiedades_austres), extra_css = "border-bottom: 1px solid black;")

# Serie, ACF, PACF
datos_austres <- leer_serie(
  austres,
  fuente = "R",
  unidad = "miles de personas"
)

grafico_austres <- graficar_serie(
  datos_austres,
  "Población australiana"
)

cor_austres <- correlograma(
  datos_austres$y
)

panel_austres <- panel_diagnostico(
  grafico_austres,
  cor_austres
)
print(panel_austres)

#Contraste Individual
r_austres <- cor_austres$acf

T_austres <- length(datos_austres$y)

z_austres <- r_austres * sqrt(T_austres)

valor_p_austres <- 2 * pnorm(-abs(z_austres))

valor_critico_austres <- qnorm(0.975)

tabla_acf_austres <- data.frame(
  rezago = 1:length(r_austres),
  r_h = r_austres,
  estadistico = z_austres,
  valor_p = valor_p_austres,
  significativo = abs(z_austres) > valor_critico_austres
)

# Ajustar el data frame para mejorar la presentación numérica
tabla_acf_austres_formato <- tabla_acf_austres
tabla_acf_austres_formato$r_h         <- round(tabla_acf_austres_formato$r_h, 4)
tabla_acf_austres_formato$estadistico <- round(tabla_acf_austres_formato$estadistico, 4)
tabla_acf_austres_formato$valor_p     <- format.pval(tabla_acf_austres_formato$valor_p, digits = 4, eps = 0.0001)
tabla_acf_austres_formato$significativo <- ifelse(tabla_acf_austres_formato$significativo, "Sí", "No")

#Visualización estilo APA
kable(tabla_acf_austres_formato,
      format    = "html",
      col.names = c("Rezago ($h$)", "Autocorrelación ($r_h$)", "Estadístico ($z$)", "Valor $p$", "Significativo"),
      align     = c("c", "c", "c", "c", "c"),
      caption   = "Contraste individual de autocorrelación — Serie de Población Australiana") |>
  kable_styling(bootstrap_options = "basic",
                full_width         = FALSE,
                position          = "center") |>
  row_spec(0, bold = TRUE) |>
  row_spec(1:nrow(tabla_acf_austres_formato), extra_css = "border-bottom: none;") |>
  column_spec(1:5, border_left = FALSE, border_right = FALSE) |>
  add_header_above(c(" " = 5), line = TRUE, bold = TRUE) |>
  row_spec(nrow(tabla_acf_austres_formato), extra_css = "border-bottom: 1px solid black;")

#Ljung Box
resultado_lb_austres <- ljung_box(
  r = cor_austres$acf,
  T = length(datos_austres$y),
  m = length(cor_austres$acf),   
  p = 0
)

cat(
  "Población australiana → Q =", round(resultado_lb_austres$estadistico, 4),
  "| gl =", resultado_lb_austres$grados_libertad,
  "| valor crítico =", round(resultado_lb_austres$valor_critico, 4),
  "| p-valor =", round(resultado_lb_austres$valor_p, 4), "\n"
)

#Partición de la serie
h_austres <- min(
  12,
  floor(0.2 * T_austres)
)

y_estimacion_austres <- datos_austres$y[1:(T_austres - h_austres)]

y_validacion_austres <- datos_austres$y[(T_austres - h_austres + 1):T_austres]

cat(
  "Población australiana → Estimación =", length(y_estimacion_austres),
  "| Validación =", length(y_validacion_austres), "\n"
)

#Ajustar Tendencia Lineal
modelo_austres <- ajustar_tendencia(
  y_estimacion_austres,
  "lineal"
)

#Pronósticos
pronosticos_austres <- modelo_austres$pronosticar(h_austres)

cat(
  "Horizonte de validación (h):", h_austres, "\n",
  "Pronósticos:", round(pronosticos_austres, 4), "\n"
)

#Medidas de error
posiciones_austres <- which(
  !is.na(modelo_austres$yhat)
)

medidas_estimacion_austres <- medidas(
  y_estimacion_austres[posiciones_austres],
  modelo_austres$yhat[posiciones_austres]
)

medidas_validacion_austres <- medidas(
  y_validacion_austres,
  pronosticos_austres
)

# Pronósticos del método ingenuo
pronosticos_ingenuo_austres <- rep(
  y_estimacion_austres[length(y_estimacion_austres)],
  h_austres
)

# Medidas de error del ingenuo
medidas_ingenuo_austres <- medidas(
  y = y_validacion_austres,
  yhat = pronosticos_ingenuo_austres
)

# Escala del MASE usando el ingenuo en estimación
escala_mase_austres <- mean(
  abs(
    y_estimacion_austres[2:length(y_estimacion_austres)] -
      y_estimacion_austres[1:(length(y_estimacion_austres) - 1)]
  )
)

# MASE correcto de cada método (estimación y validación)
mase_estim_austres <- medidas_estimacion_austres$MAD / escala_mase_austres
mase_valid_austres <- medidas_validacion_austres$MAD / escala_mase_austres
mase_ingenuo_austres <- medidas_ingenuo_austres$MAD / escala_mase_austres

# Tabla única de comparación
tabla_medidas_austres <- data.frame(
  Tramo = c("Estimación", "Validación", "Validación"),
  Método = c("Tendencia lineal", "Tendencia lineal", "Ingenuo"),
  MSE = c(
    medidas_estimacion_austres$MSE,
    medidas_validacion_austres$MSE,
    medidas_ingenuo_austres$MSE
  ),
  MAD = c(
    medidas_estimacion_austres$MAD,
    medidas_validacion_austres$MAD,
    medidas_ingenuo_austres$MAD
  ),
  MAPE = c(
    medidas_estimacion_austres$MAPE,
    medidas_validacion_austres$MAPE,
    medidas_ingenuo_austres$MAPE
  ),
  MASE = c(
    mase_estim_austres,
    mase_valid_austres,
    mase_ingenuo_austres
  )
)

tabla_medidas_austres

# Errores de pronóstico dentro de la estimación

posiciones_austres <- which(!is.na(modelo_austres$yhat))

errores_austres <- y_estimacion_austres[posiciones_austres] - modelo_austres$yhat[posiciones_austres]

#Validación de los errores
resultado_errores_austres <- validar_errores(
  e = errores_austres,
  m = NULL,
  p = 2    # tendencia lineal: 2 parámetros (intercepto y pendiente)
)

#Grafico errores
resultado_errores_austres$grafico_errores

#ACF, PACF
resultado_errores_austres$correlograma$grafico

# Residuos vs valores ajustados
datos_residuos_austres <- data.frame(
  ajustado = modelo_austres$yhat,
  residuo = modelo_austres$parametros$residuos
)

ggplot2::ggplot(
  datos_residuos_austres,
  ggplot2::aes(x = ajustado, y = residuo)
) +
  ggplot2::geom_point(color = "purple") +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::labs(
    title = "Residuos vs valores ajustados - Población australiana",
    x = "Valor ajustado",
    y = "Residuo"
  ) +
  ggplot2::theme_minimal()

#Prueba t errores
cat(
  "Estadístico de prueba:", round(resultado_errores_austres$t, 4), "\n",
  "Grados de libertad:", resultado_errores_austres$grados_libertad_t, "\n",
  "Valor crítico:", round(
    qt(0.975, resultado_errores_austres$grados_libertad_t), 4
  ), "\n",
  "Valor p:", round(resultado_errores_austres$valor_p_t, 4), "\n"
)

#Ljung Box
cat(
  "Estadístico de prueba:", round(
    resultado_errores_austres$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_austres$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_austres$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    resultado_errores_austres$ljung_box$valor_p, 4
  ), "\n"
)

#Jarque Bera
cat(
  "Estadístico de prueba:", round(
    resultado_errores_austres$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_austres$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_austres$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    resultado_errores_austres$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

#Durbin Watson

# Valores críticos de Durbin-Watson al 5%
tabla_dw <- data.frame(
  n = c(15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100),
  dL = c(1.08, 1.20, 1.29, 1.35, 1.44, 1.50, 1.55, 1.58, 1.61, 1.63, 1.65),
  dU = c(1.36, 1.41, 1.45, 1.49, 1.54, 1.59, 1.62, 1.64, 1.66, 1.68, 1.69)
)

# Número de residuos
n <- length(modelo_austres$parametros$residuos)

# Valores críticos aproximados para el n de la serie
dL <- approx(tabla_dw$n, tabla_dw$dL, xout = n)$y
dU <- approx(tabla_dw$n, tabla_dw$dU, xout = n)$y

# Estadístico Durbin-Watson
DW <- modelo_austres$parametros$DW

cat("n =", n, "\n")
cat("DW =", round(DW, 4), "\n")
cat("dL =", round(dL, 4), "\n")
cat("dU =", round(dU, 4), "\n")
cat("4 - dU =", round(4 - dU, 4), "\n")
cat("4 - dL =", round(4 - dL, 4), "\n")

#Prueba coeficientes
tabla_coef_austres <- modelo_austres$parametros$coeficientes

tabla_coef_austres$estimacion   <- round(tabla_coef_austres$estimacion, 4)
tabla_coef_austres$se_ordinario <- round(tabla_coef_austres$se_ordinario, 4)
tabla_coef_austres$se_robusto   <- round(tabla_coef_austres$se_robusto, 4)
tabla_coef_austres$t            <- round(tabla_coef_austres$t, 4)
tabla_coef_austres$valor_p      <- format.pval(tabla_coef_austres$valor_p, digits = 4, eps = 0.0001)

tabla_coef_austres

#Grafico final
ggplot2::ggplot(
  data.frame(
    tiempo = 1:T_austres,
    real = datos_austres$y,
    ajuste = c(modelo_austres$yhat, rep(NA, h_austres)),
    pronostico = c(rep(NA, T_austres - h_austres), pronosticos_austres)
  ),
  ggplot2::aes(x = tiempo)
) +
  
  # Serie real
  ggplot2::geom_line(
    ggplot2::aes(y = real),
    color = "purple"
  ) +
  
  # Ajuste de tendencia lineal
  ggplot2::geom_line(
    ggplot2::aes(y = ajuste),
    color = "green"
  ) +
  
  # Pronóstico
  ggplot2::geom_line(
    ggplot2::aes(y = pronostico),
    color = "green"
  ) +
  
  # Separación entre estimación y validación
  ggplot2::geom_vline(
    xintercept = T_austres - h_austres,
    color = "red",
    linetype = "dashed"
  ) +
  
  ggplot2::labs(
    title = "Población australiana: ajuste y pronóstico de tendencia lineal",
    x = "Tiempo",
    y = "Población (miles de personas)"
  ) +
  
  ggplot2::theme_minimal()


# TENDENCIA CUADRATICA - MILLAS PASAJEROS

#Descripción de la serie
propiedades_airmiles <- data.frame(
  Métrica    = c("Longitud de la serie (n)", 
                 "Año de inicio", 
                 "Año de fin", 
                 "Frecuencia observada"),
  Valor      = c(length(datos_airmiles$y),
                 start(airmiles)[1],
                 end(airmiles)[1],
                 frequency(airmiles)),
  Descripción = c("Número total de observaciones anuales",
                  "Primer año registrado en la serie",
                  "Último año registrado en la serie",
                  "Frecuencia anual de medición (1 dato/año)"),
  stringsAsFactors = FALSE
)

kable(propiedades_airmiles,
      format    = "html",
      col.names = c("Métrica", "Valor", "Descripción"),
      align     = c("l", "c", "l"),
      caption   = "Propiedades temporales — Serie de Millas de Pasajeros") |>
  kable_styling(bootstrap_options = "basic",
                full_width         = FALSE,
                position          = "center") |>
  row_spec(0, bold = TRUE) |>
  row_spec(1:nrow(propiedades_airmiles), extra_css = "border-bottom: none;") |>
  column_spec(1:3, border_left = FALSE, border_right = FALSE) |>
  add_header_above(c(" " = 3), line = TRUE, bold = TRUE) |>
  row_spec(nrow(propiedades_airmiles), extra_css = "border-bottom: 1px solid black;")

#Serie, ACF, PACF
datos_airmiles <- leer_serie(
  airmiles,
  fuente = "R",
  unidad = "millas"
)

grafico_airmiles <- graficar_serie(
  datos_airmiles,
  "Millas de pasajeros"
)

cor_airmiles <- correlograma(
  datos_airmiles$y
)

panel_airmiles <- panel_diagnostico(
  grafico_airmiles,
  cor_airmiles
)

print(panel_airmiles)

#Constrastes Individuales
r_airmiles <- cor_airmiles$acf

T_airmiles <- length(datos_airmiles$y)

z_airmiles <- r_airmiles * sqrt(T_airmiles)

valor_p_airmiles <- 2 * pnorm(-abs(z_airmiles))

valor_critico_airmiles <- qnorm(0.975)

tabla_acf_airmiles <- data.frame(
  rezago = 1:length(r_airmiles),
  r_h = r_airmiles,
  estadistico = z_airmiles,
  valor_p = valor_p_airmiles,
  significativo = abs(z_airmiles) > valor_critico_airmiles
)

tabla_acf_airmiles_formato <- tabla_acf_airmiles
tabla_acf_airmiles_formato$r_h         <- round(tabla_acf_airmiles_formato$r_h, 4)
tabla_acf_airmiles_formato$estadistico <- round(tabla_acf_airmiles_formato$estadistico, 4)
tabla_acf_airmiles_formato$valor_p     <- format.pval(tabla_acf_airmiles_formato$valor_p, digits = 4, eps = 0.0001)
tabla_acf_airmiles_formato$significativo <- ifelse(tabla_acf_airmiles_formato$significativo, "Sí", "No")

#Visualizacion
kable(tabla_acf_airmiles_formato,
      format    = "html",
      col.names = c("Rezago ($h$)", "Autocorrelación ($r_h$)", "Estadístico ($z$)", "Valor $p$", "Significativo"),
      align     = c("c", "c", "c", "c", "c"),
      caption   = "Contraste individual de autocorrelación — Serie de Millas de Pasajeros") |>
  kable_styling(bootstrap_options = "basic",
                full_width         = FALSE,
                position          = "center") |>
  row_spec(0, bold = TRUE) |>
  row_spec(1:nrow(tabla_acf_airmiles_formato), extra_css = "border-bottom: none;") |>
  column_spec(1:5, border_left = FALSE, border_right = FALSE) |>
  add_header_above(c(" " = 5), line = TRUE, bold = TRUE) |>
  row_spec(nrow(tabla_acf_airmiles_formato), extra_css = "border-bottom: 1px solid black;")

#Ljung Box
resultado_lb_airmiles <- ljung_box(
  r = cor_airmiles$acf,
  T = length(datos_airmiles$y),
  m = length(cor_airmiles$acf),
  p = 0
)

cat(
  "Millas de pasajeros → Q =", round(resultado_lb_airmiles$estadistico, 4),
  "| gl =", resultado_lb_airmiles$grados_libertad,
  "| valor crítico =", round(resultado_lb_airmiles$valor_critico, 4),
  "| p-valor =", round(resultado_lb_airmiles$valor_p, 4), "\n"
)

#Particion de la serie
h_airmiles <- min(
  12,
  floor(0.2 * T_airmiles)
)

y_estimacion_airmiles <- datos_airmiles$y[1:(T_airmiles - h_airmiles)]

y_validacion_airmiles <- datos_airmiles$y[(T_airmiles - h_airmiles + 1):T_airmiles]

cat(
  "Millas de pasajeros → Estimación =", length(y_estimacion_airmiles),
  "| Validación =", length(y_validacion_airmiles), "\n"
)

#Ajustar Tendencia cuadratica
modelo_airmiles <- ajustar_tendencia(
  y_estimacion_airmiles,
  "cuadratica"
)

pronosticos_airmiles <- modelo_airmiles$pronosticar(h_airmiles)

cat(
  "Horizonte de validación (h):", h_airmiles, "\n",
  "Pronósticos:", round(pronosticos_airmiles, 4), "\n"
)

# Medidas de error en el período de estimación (ajuste)
posiciones_airmiles <- which(!is.na(modelo_airmiles$yhat))

medidas_estimacion_airmiles <- medidas(
  y = y_estimacion_airmiles[posiciones_airmiles],
  yhat = modelo_airmiles$yhat[posiciones_airmiles]
)

# Medidas de error en el período de validación
medidas_validacion_airmiles <- medidas(
  y = y_validacion_airmiles,
  yhat = pronosticos_airmiles
)

# Pronósticos del método ingenuo
pronosticos_ingenuo_airmiles <- rep(
  y_estimacion_airmiles[length(y_estimacion_airmiles)],
  h_airmiles
)

# Medidas de error del ingenuo
medidas_ingenuo_airmiles <- medidas(
  y = y_validacion_airmiles,
  yhat = pronosticos_ingenuo_airmiles
)

# Escala del MASE usando el ingenuo en estimación
escala_mase_airmiles <- mean(
  abs(
    y_estimacion_airmiles[2:length(y_estimacion_airmiles)] -
      y_estimacion_airmiles[1:(length(y_estimacion_airmiles) - 1)]
  )
)

# MASE correcto de cada método (estimación y validación)
mase_estim_airmiles <- medidas_estimacion_airmiles$MAD / escala_mase_airmiles
mase_valid_airmiles <- medidas_validacion_airmiles$MAD / escala_mase_airmiles
mase_ingenuo_airmiles <- medidas_ingenuo_airmiles$MAD / escala_mase_airmiles

# Tabla única de comparación
tabla_medidas_airmiles <- data.frame(
  Tramo = c("Estimación", "Validación", "Validación"),
  Método = c("Tendencia cuadrática", "Tendencia cuadrática", "Ingenuo"),
  MSE = c(
    medidas_estimacion_airmiles$MSE,
    medidas_validacion_airmiles$MSE,
    medidas_ingenuo_airmiles$MSE
  ),
  MAD = c(
    medidas_estimacion_airmiles$MAD,
    medidas_validacion_airmiles$MAD,
    medidas_ingenuo_airmiles$MAD
  ),
  MAPE = c(
    medidas_estimacion_airmiles$MAPE,
    medidas_validacion_airmiles$MAPE,
    medidas_ingenuo_airmiles$MAPE
  ),
  MASE = c(
    mase_estim_airmiles,
    mase_valid_airmiles,
    mase_ingenuo_airmiles
  )
)

tabla_medidas_airmiles

#Validación de los errores
posiciones_airmiles <- which(!is.na(modelo_airmiles$yhat))

errores_airmiles <- y_estimacion_airmiles[posiciones_airmiles] - modelo_airmiles$yhat[posiciones_airmiles]

resultado_errores_airmiles <- validar_errores(
  e = errores_airmiles,
  m = NULL,
  p = 3    # tendencia cuadrática: 3 parámetros (intercepto, lineal, cuadrático)
)

#Grafico errores
resultado_errores_airmiles$grafico_errores

#ACF, PACF
resultado_errores_airmiles$correlograma$grafico

# Residuos vs valores ajustados
datos_residuos_airmiles <- data.frame(
  ajustado = modelo_airmiles$yhat[posiciones_airmiles],
  residuo = errores_airmiles
)

ggplot2::ggplot(
  datos_residuos_airmiles,
  ggplot2::aes(x = ajustado, y = residuo)
) +
  ggplot2::geom_point(color = "purple") +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::labs(
    title = "Residuos vs valores ajustados - Millas de pasajeros",
    x = "Valor ajustado",
    y = "Residuo"
  ) +
  ggplot2::theme_minimal()

#Prueba t errores
cat(
  "Estadístico de prueba:", round(resultado_errores_airmiles$t, 4), "\n",
  "Grados de libertad:", resultado_errores_airmiles$grados_libertad_t, "\n",
  "Valor crítico:", round(
    qt(0.975, resultado_errores_airmiles$grados_libertad_t), 4
  ), "\n",
  "Valor p:", round(resultado_errores_airmiles$valor_p_t, 4), "\n"
)

#Ljung Box
cat(
  "Estadístico de prueba:", round(
    resultado_errores_airmiles$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_airmiles$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_airmiles$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    resultado_errores_airmiles$ljung_box$valor_p, 4
  ), "\n"
)

#Jarque Bera
cat(
  "Estadístico de prueba:", round(
    resultado_errores_airmiles$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_airmiles$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_airmiles$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    resultado_errores_airmiles$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

#Durbin Watson

# Valores críticos de Durbin-Watson al 5%
tabla_dw <- data.frame(
  n = c(15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100),
  dL = c(1.08, 1.20, 1.29, 1.35, 1.44, 1.50, 1.55, 1.58, 1.61, 1.63, 1.65),
  dU = c(1.36, 1.41, 1.45, 1.49, 1.54, 1.59, 1.62, 1.64, 1.66, 1.68, 1.69)
)

# Número de errores
n <- length(modelo_airmiles$parametros$residuos)

# Valores críticos aproximados
dL <- approx(tabla_dw$n, tabla_dw$dL, xout = n)$y
dU <- approx(tabla_dw$n, tabla_dw$dU, xout = n)$y

# Estadístico Durbin-Watson
DW <- modelo_airmiles$parametros$DW

cat("n =", n, "\n")
cat("DW =", round(DW, 4), "\n")
cat("dL =", round(dL, 4), "\n")
cat("dU =", round(dU, 4), "\n")
cat("4 - dU =", round(4 - dU, 4), "\n")
cat("4 - dL =", round(4 - dL, 4), "\n")

#Pruebas t sobre los coeficientes (error estándar robusto)
tabla_coef_airmiles <- modelo_airmiles$parametros$coeficientes

tabla_coef_airmiles$estimacion   <- round(tabla_coef_airmiles$estimacion, 4)
tabla_coef_airmiles$se_ordinario <- round(tabla_coef_airmiles$se_ordinario, 4)
tabla_coef_airmiles$se_robusto   <- round(tabla_coef_airmiles$se_robusto, 4)
tabla_coef_airmiles$t            <- round(tabla_coef_airmiles$t, 4)
tabla_coef_airmiles$valor_p      <- format.pval(tabla_coef_airmiles$valor_p, digits = 4, eps = 0.0001)

tabla_coef_airmiles

#Grafico final
ggplot2::ggplot(
  data.frame(
    tiempo = 1:T_airmiles,
    real = datos_airmiles$y,
    ajuste = c(modelo_airmiles$yhat, rep(NA, h_airmiles)),
    pronostico = c(rep(NA, T_airmiles - h_airmiles), pronosticos_airmiles)
  ),
  ggplot2::aes(x = tiempo)
) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = real),
    color = "purple"
  ) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = ajuste),
    color = "green"
  ) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = pronostico),
    color = "green"
  ) +
  
  ggplot2::geom_vline(
    xintercept = T_airmiles - h_airmiles,
    color = "red",
    linetype = "dashed"
  ) +
  
  ggplot2::labs(
    title = "Millas de pasajeros: ajuste y pronóstico de tendencia cuadrática",
    x = "Tiempo",
    y = "Millas de pasajeros"
  ) +
  
  ggplot2::theme_minimal()


# TENDENCIA EXPONENCIAL - JOHNSON & JOHNSON

#Descripción de la serie
propiedades_J <- data.frame(
  Métrica    = c("Longitud de la serie (n)", 
                 "Año de inicio", 
                 "Año de fin", 
                 "Frecuencia observada"),
  Valor      = c(length(datos_J$y),
                 start(JohnsonJohnson)[1],
                 end(JohnsonJohnson)[1],
                 frequency(JohnsonJohnson)),
  Descripción = c("Número total de observaciones trimestrales",
                  "Primer año registrado en la serie",
                  "Último año registrado en la serie",
                  "Frecuencia trimestral de medición (4 datos/año)"),
  stringsAsFactors = FALSE
)

propiedades_J

#Serie, ACF, PACF
datos_J <- leer_serie(
  JohnsonJohnson,
  fuente = "R",
  unidad = "millones de dólares"
)

grafico_J <- graficar_serie(
  datos_J,
  "Ganancias de Johnson & Johnson"
)

cor_J <- correlograma(
  datos_J$y
)

panel_J <- panel_diagnostico(
  grafico_J,
  cor_J
)

print(panel_J)

#Constraste individual
r_J <- cor_J$acf

T_J <- length(datos_J$y)

z_J <- r_J * sqrt(T_J)

valor_p_J <- 2 * pnorm(-abs(z_J))

valor_critico_J <- qnorm(0.975)

tabla_acf_J <- data.frame(
  rezago = 1:length(r_J),
  r_h = r_J,
  estadistico = z_J,
  valor_p = valor_p_J,
  significativo = abs(z_J) > valor_critico_J
)

tabla_acf_J$r_h         <- round(tabla_acf_J$r_h, 4)
tabla_acf_J$estadistico <- round(tabla_acf_J$estadistico, 4)
tabla_acf_J$valor_p     <- format.pval(tabla_acf_J$valor_p, digits = 4, eps = 0.0001)
tabla_acf_J$significativo <- ifelse(tabla_acf_J$significativo, "Sí", "No")

tabla_acf_J

#Lung Box
resultado_lb_J <- ljung_box(
  r = cor_J$acf,
  T = length(datos_J$y),
  m = length(cor_J$acf),
  p = 0
)

cat(
  "Johnson & Johnson → Q =", round(resultado_lb_J$estadistico, 4),
  "| gl =", resultado_lb_J$grados_libertad,
  "| valor crítico =", round(resultado_lb_J$valor_critico, 4),
  "| p-valor =", round(resultado_lb_J$valor_p, 4), "\n"
)

#Particion de la serie
h_J <- min(
  12,
  floor(0.2 * T_J)
)

y_estimacion_J <- datos_J$y[1:(T_J - h_J)]

y_validacion_J <- datos_J$y[(T_J - h_J + 1):T_J]

cat(
  "Johnson & Johnson → Estimación =", length(y_estimacion_J),
  "| Validación =", length(y_validacion_J), "\n"
)

#Ajustar el modelo
modelo_J <- ajustar_tendencia(
  y_estimacion_J,
  "exponencial"
)

pronosticos_J <- modelo_J$pronosticar(h_J)

cat(
  "Horizonte de validación (h):", h_J, "\n",
  "Pronósticos:", round(pronosticos_J, 4), "\n"
)

# Medidas de error en el período de estimación (ajuste)
posiciones_J <- which(!is.na(modelo_J$yhat))

medidas_estimacion_J <- medidas(
  y = y_estimacion_J[posiciones_J],
  yhat = modelo_J$yhat[posiciones_J]
)

# Medidas de error en el período de validación
medidas_validacion_J <- medidas(
  y = y_validacion_J,
  yhat = pronosticos_J
)

# Pronósticos del método ingenuo
pronosticos_ingenuo_J <- rep(
  y_estimacion_J[length(y_estimacion_J)],
  h_J
)

# Medidas de error del ingenuo
medidas_ingenuo_J <- medidas(
  y = y_validacion_J,
  yhat = pronosticos_ingenuo_J
)

# Escala del MASE usando el ingenuo en estimación
escala_mase_J <- mean(
  abs(
    y_estimacion_J[2:length(y_estimacion_J)] -
      y_estimacion_J[1:(length(y_estimacion_J) - 1)]
  )
)

# MASE correcto de cada método (estimación y validación)
mase_estim_J <- medidas_estimacion_J$MAD / escala_mase_J
mase_valid_J <- medidas_validacion_J$MAD / escala_mase_J
mase_ingenuo_J <- medidas_ingenuo_J$MAD / escala_mase_J

# Tabla única de comparación
tabla_medidas_J <- data.frame(
  Tramo = c("Estimación", "Validación", "Validación"),
  Método = c("Tendencia exponencial", "Tendencia exponencial", "Ingenuo"),
  MSE = c(
    medidas_estimacion_J$MSE,
    medidas_validacion_J$MSE,
    medidas_ingenuo_J$MSE
  ),
  MAD = c(
    medidas_estimacion_J$MAD,
    medidas_validacion_J$MAD,
    medidas_ingenuo_J$MAD
  ),
  MAPE = c(
    medidas_estimacion_J$MAPE,
    medidas_validacion_J$MAPE,
    medidas_ingenuo_J$MAPE
  ),
  MASE = c(
    mase_estim_J,
    mase_valid_J,
    mase_ingenuo_J
  )
)

tabla_medidas_J

# Validación de los errores
posiciones_J <- which(!is.na(modelo_J$yhat))

errores_J <- y_estimacion_J[posiciones_J] - modelo_J$yhat[posiciones_J]

resultado_errores_J <- validar_errores(
  e = errores_J,
  m = NULL,
  p = 2    # tendencia exponencial: 2 parámetros (intercepto y tasa, sobre log Y)
)


# Gráfico errores
resultado_errores_J$grafico_errores

#ACF, PACF
resultado_errores_J$correlograma$grafico

# Residuos vs valores ajustados
datos_residuos_J <- data.frame(
  ajustado = modelo_J$yhat[posiciones_J],
  residuo = errores_J
)

ggplot2::ggplot(
  datos_residuos_J,
  ggplot2::aes(x = ajustado, y = residuo)
) +
  ggplot2::geom_point(color = "purple") +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::labs(
    title = "Residuos vs valores ajustados - Johnson & Johnson",
    x = "Valor ajustado",
    y = "Residuo"
  ) +
  ggplot2::theme_minimal()

#Prueba t errores
cat(
  "Estadístico de prueba:", round(resultado_errores_J$t, 4), "\n",
  "Grados de libertad:", resultado_errores_J$grados_libertad_t, "\n",
  "Valor crítico:", round(
    qt(0.975, resultado_errores_J$grados_libertad_t), 4
  ), "\n",
  "Valor p:", round(resultado_errores_J$valor_p_t, 4), "\n"
)

#Ljung Box
cat(
  "Estadístico de prueba:", round(
    resultado_errores_J$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_J$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_J$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    resultado_errores_J$ljung_box$valor_p, 4
  ), "\n"
)

#Jarque Bera
cat(
  "Estadístico de prueba:", round(
    resultado_errores_J$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_J$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_J$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    resultado_errores_J$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

#Durbin Watson
# Valores críticos de Durbin-Watson al 5%
tabla_dw <- data.frame(
  n = c(15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100),
  dL = c(1.08, 1.20, 1.29, 1.35, 1.44, 1.50, 1.55, 1.58, 1.61, 1.63, 1.65),
  dU = c(1.36, 1.41, 1.45, 1.49, 1.54, 1.59, 1.62, 1.64, 1.66, 1.68, 1.69)
)

# Número de errores
n <- length(modelo_J$parametros$residuos)

# Valores críticos aproximados
dL <- approx(tabla_dw$n, tabla_dw$dL, xout = n)$y
dU <- approx(tabla_dw$n, tabla_dw$dU, xout = n)$y

# Estadístico Durbin-Watson
DW <- modelo_J$parametros$DW

cat("n =", n, "\n")
cat("DW =", round(DW, 4), "\n")
cat("dL =", round(dL, 4), "\n")
cat("dU =", round(dU, 4), "\n")
cat("4 - dU =", round(4 - dU, 4), "\n")
cat("4 - dL =", round(4 - dL, 4), "\n")

#Prueba coeficientes
tabla_coef_J <- modelo_J$parametros$coeficientes

tabla_coef_J$estimacion   <- round(tabla_coef_J$estimacion, 4)
tabla_coef_J$se_ordinario <- round(tabla_coef_J$se_ordinario, 4)
tabla_coef_J$se_robusto   <- round(tabla_coef_J$se_robusto, 4)
tabla_coef_J$t            <- round(tabla_coef_J$t, 4)
tabla_coef_J$valor_p      <- format.pval(tabla_coef_J$valor_p, digits = 4, eps = 0.0001)

tabla_coef_J

n_J <- length(y_estimacion_J)

cat(
  "Grados de libertad:", n_J - 2, "\n",
  "Valor crítico:", round(qt(0.975, n_J - 2), 4), "\n"
)

#Grafico final
ggplot2::ggplot(
  data.frame(
    tiempo = 1:T_J,
    real = datos_J$y,
    ajuste = c(modelo_J$yhat, rep(NA, h_J)),
    pronostico = c(rep(NA, T_J - h_J), pronosticos_J)
  ),
  ggplot2::aes(x = tiempo)
) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = real),
    color = "purple"
  ) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = ajuste),
    color = "green"
  ) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = pronostico),
    color = "green"
  ) +
  
  ggplot2::geom_vline(
    xintercept = T_J - h_J,
    color = "red",
    linetype = "dashed"
  ) +
  
  ggplot2::labs(
    title = "Ganancias de Johnson & Johnson: ajuste y pronóstico de tendencia exponencial",
    x = "Tiempo",
    y = "Ganancias (millones de dólares)"
  ) +
  
  ggplot2::theme_minimal()


# HOLT LINEAL - USO DE INTERNET

#Descripción de la serie

propiedades_W <- data.frame(
  Métrica    = c("Longitud de la serie (n)", 
                 "Fecha/periodo de inicio", 
                 "Fecha/periodo de fin", 
                 "Frecuencia observada"),
  Valor      = c(length(datos_W$y),
                 start(WWWusage)[1],
                 end(WWWusage)[1],
                 frequency(WWWusage)),
  Descripción = c("Número total de observaciones",
                  "Primer periodo registrado en la serie",
                  "Último periodo registrado en la serie",
                  "Frecuencia de medición (1 dato/minuto)"),
  stringsAsFactors = FALSE
)

propiedades_W

#Serie, ACF, PACF
datos_W <- leer_serie(
  WWWusage,
  fuente = "R",
  unidad = "usuarios"
)

grafico_W <- graficar_serie(
  datos_W,
  "Uso de Internet"
)

cor_W <- correlograma(
  datos_W$y
)

panel_W <- panel_diagnostico(
  grafico_W,
  cor_W
)

print(panel_W)

#Contaste individual
r_W <- cor_W$acf

T_W <- length(datos_W$y)

z_W <- r_W * sqrt(T_W)

valor_p_W <- 2 * pnorm(-abs(z_W))

valor_critico_W <- qnorm(0.975)

tabla_acf_W <- data.frame(
  rezago = 1:length(r_W),
  r_h = r_W,
  estadistico = z_W,
  valor_p = valor_p_W,
  significativo = abs(z_W) > valor_critico_W
)

tabla_acf_W$r_h         <- round(tabla_acf_W$r_h, 4)
tabla_acf_W$estadistico <- round(tabla_acf_W$estadistico, 4)
tabla_acf_W$valor_p     <- format.pval(tabla_acf_W$valor_p, digits = 4, eps = 0.0001)
tabla_acf_W$significativo <- ifelse(tabla_acf_W$significativo, "Sí", "No")

tabla_acf_W

#Lung box
resultado_lb_W <- ljung_box(
  r = cor_W$acf,
  T = length(datos_W$y),
  m = length(cor_W$acf),
  p = 0
)

cat(
  "Uso de Internet → Q =", round(resultado_lb_W$estadistico, 4),
  "| gl =", resultado_lb_W$grados_libertad,
  "| valor crítico =", round(resultado_lb_W$valor_critico, 4),
  "| p-valor =", round(resultado_lb_W$valor_p, 4), "\n"
)

#Particion
h_W <- min(
  12,
  floor(0.2 * T_W)
)

y_estimacion_W <- datos_W$y[1:(T_W - h_W)]

y_validacion_W <- datos_W$y[(T_W - h_W + 1):T_W]

cat(
  "Uso de Internet → Estimación =", length(y_estimacion_W),
  "| Validación =", length(y_validacion_W), "\n"
)

# Optimización de la ventana de la media móvil
resultado_opt_W <- optimizar(
  y = y_estimacion_W,
  metodo = "holt",
  rejilla = seq(0.1, 0.9, by = 0.1)
)

resultado_opt_W$rejilla

#Mapra optimizacion Holt
grafico_opt_W <- graficar_holt(
  resultado = resultado_opt_W
)

grafico_opt_W

# Ajustar modelo
modelo_W <- ajustar_holt(
  y_estimacion_W,
  resultado_opt_W$optimo$alpha,
  resultado_opt_W$optimo$beta
)

pronosticos_W <- modelo_W$pronosticar(h_W)

cat(
  "Horizonte de validación (h):", h_W, "\n",
  "Alpha óptimo:", resultado_opt_W$optimo$alpha, "\n",
  "Beta óptimo:", resultado_opt_W$optimo$beta, "\n",
  "Pronósticos:", round(pronosticos_W, 4), "\n"
)

# Medidas de error
posiciones_W <- which(!is.na(modelo_W$yhat))

medidas_estimacion_W <- medidas(
  y = y_estimacion_W[posiciones_W],
  yhat = modelo_W$yhat[posiciones_W]
)

medidas_validacion_W <- medidas(
  y = y_validacion_W,
  yhat = pronosticos_W
)

#Ingenuo
pronosticos_ingenuo_W <- rep(
  y_estimacion_W[length(y_estimacion_W)],
  h_W
)

medidas_ingenuo_W <- medidas(
  y = y_validacion_W,
  yhat = pronosticos_ingenuo_W
)

#Escala MASE
escala_mase_W <- mean(
  abs(
    y_estimacion_W[2:length(y_estimacion_W)] -
      y_estimacion_W[1:(length(y_estimacion_W) - 1)]
  )
)

mase_estim_W <- medidas_estimacion_W$MAD / escala_mase_W
mase_valid_W <- medidas_validacion_W$MAD / escala_mase_W
mase_ingenuo_W <- medidas_ingenuo_W$MAD / escala_mase_W

tabla_medidas_W <- data.frame(
  Tramo = c("Estimación", "Validación", "Validación"),
  Método = c("Holt lineal", "Holt lineal", "Ingenuo"),
  MSE = c(
    medidas_estimacion_W$MSE,
    medidas_validacion_W$MSE,
    medidas_ingenuo_W$MSE
  ),
  MAD = c(
    medidas_estimacion_W$MAD,
    medidas_validacion_W$MAD,
    medidas_ingenuo_W$MAD
  ),
  MAPE = c(
    medidas_estimacion_W$MAPE,
    medidas_validacion_W$MAPE,
    medidas_ingenuo_W$MAPE
  ),
  MASE = c(
    mase_estim_W,
    mase_valid_W,
    mase_ingenuo_W
  )
)

tabla_medidas_W

#Validación de los errores
posiciones_W <- which(!is.na(modelo_W$yhat))

errores_W <- y_estimacion_W[posiciones_W] - modelo_W$yhat[posiciones_W]

resultado_errores_W <- validar_errores(
  e = errores_W,
  m = NULL,
  p = 2
)

#Gráfico errores
resultado_errores_W$grafico_errores

#ACF, PACF
resultado_errores_W$correlograma$grafico

#Prueba t errores
cat(
  "Estadístico de prueba:", round(resultado_errores_W$t, 4), "\n",
  "Grados de libertad:", resultado_errores_W$grados_libertad_t, "\n",
  "Valor crítico:", round(
    qt(0.975, resultado_errores_W$grados_libertad_t), 4
  ), "\n",
  "Valor p:", round(resultado_errores_W$valor_p_t, 4), "\n"
)

#Ljung Box errores
cat(
  "Estadístico de prueba:", round(
    resultado_errores_W$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_W$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_W$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    resultado_errores_W$ljung_box$valor_p, 4
  ), "\n"
)

#Jarque Bera errores
cat(
  "Estadístico de prueba:", round(
    resultado_errores_W$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_W$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_W$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    resultado_errores_W$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

#Durbin Watson
cat(
  "Estadístico de prueba:", round(
    resultado_errores_W$durbin_watson$estadistico, 4
  ), "\n",
  "Grados de libertad: no aplica\n",
  "Valor crítico: no calculado\n",
  "Valor p: no calculado\n"
)

#Grafico final
ggplot2::ggplot(
  data.frame(
    tiempo = 1:T_W,
    real = datos_W$y,
    ajuste = c(modelo_W$yhat, rep(NA, h_W)),
    pronostico = c(rep(NA, T_W - h_W), pronosticos_W)
  ),
  ggplot2::aes(x = tiempo)
) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = real),
    color = "purple"
  ) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = ajuste),
    color = "green"
  ) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = pronostico),
    color = "green"
  ) +
  
  ggplot2::geom_vline(
    xintercept = T_W - h_W,
    color = "red",
    linetype = "dashed"
  ) +
  
  ggplot2::labs(
    title = "Uso de Internet: ajuste y pronóstico de Holt lineal",
    x = "Tiempo",
    y = "Usuarios"
  ) +
  
  ggplot2::theme_minimal()


# CONTRAEJEMPLO - POBLACION AUSTRALIA CON MEDIA MOVIL

# Se reutiliza la misma partición del ejemplo de tendencia lineal
h_austres_mm <- h_austres

y_estimacion_austres_mm <- y_estimacion_austres
y_validacion_austres_mm <- y_validacion_austres

cat(
  "Población australiana (contraejemplo) → Estimación =", length(y_estimacion_austres_mm),
  "| Validación =", length(y_validacion_austres_mm), "\n"
)

# Optimización de k
resultado_opt_austres_mm <- optimizar(
  y = y_estimacion_austres_mm,
  metodo = "mm",
  rejilla = 2:12
)

resultado_opt_austres_mm$rejilla

# Grafico optimizacion
grafico_opt_austres_mm <- graficar_optimizacion(
  resultado = resultado_opt_austres_mm,
  parametro = "k",
  titulo = "Optimización de la ventana de media móvil - Población australiana (contraejemplo)"
)

grafico_opt_austres_mm

#Ajustar Media Movil
modelo_austres_mm <- ajustar_mm(
  y = y_estimacion_austres_mm,
  k = resultado_opt_austres_mm$optimo$k
)

pronosticos_austres_mm <- modelo_austres_mm$pronosticar(h_austres_mm)

cat(
  "Horizonte de validación (h):", h_austres_mm, "\n",
  "k óptimo:", resultado_opt_austres_mm$optimo$k, "\n",
  "Pronóstico constante:", round(pronosticos_austres_mm[1], 4), "\n"
)

# Medidas de error

#Posiciones
posiciones_austres_mm <- which(!is.na(modelo_austres_mm$yhat))

medidas_estimacion_austres_mm <- medidas(
  y = y_estimacion_austres_mm[posiciones_austres_mm],
  yhat = modelo_austres_mm$yhat[posiciones_austres_mm]
)

medidas_validacion_austres_mm <- medidas(
  y = y_validacion_austres_mm,
  yhat = pronosticos_austres_mm
)

#Referente ingenuo
pronosticos_ingenuo_austres_mm <- rep(
  y_estimacion_austres_mm[length(y_estimacion_austres_mm)],
  h_austres_mm
)

medidas_ingenuo_austres_mm <- medidas(
  y = y_validacion_austres_mm,
  yhat = pronosticos_ingenuo_austres_mm
)

#Escala MASE
escala_mase_austres_mm <- mean(
  abs(
    y_estimacion_austres_mm[2:length(y_estimacion_austres_mm)] -
      y_estimacion_austres_mm[1:(length(y_estimacion_austres_mm) - 1)]
  )
)

mase_estim_austres_mm <- medidas_estimacion_austres_mm$MAD / escala_mase_austres_mm
mase_valid_austres_mm <- medidas_validacion_austres_mm$MAD / escala_mase_austres_mm
mase_ingenuo_austres_mm <- medidas_ingenuo_austres_mm$MAD / escala_mase_austres_mm

#Tabla de las medidas
tabla_medidas_austres_mm <- data.frame(
  Tramo = c("Estimación", "Validación", "Validación"),
  Método = c("Media móvil", "Media móvil", "Ingenuo"),
  MSE = c(
    medidas_estimacion_austres_mm$MSE,
    medidas_validacion_austres_mm$MSE,
    medidas_ingenuo_austres_mm$MSE
  ),
  MAD = c(
    medidas_estimacion_austres_mm$MAD,
    medidas_validacion_austres_mm$MAD,
    medidas_ingenuo_austres_mm$MAD
  ),
  MAPE = c(
    medidas_estimacion_austres_mm$MAPE,
    medidas_validacion_austres_mm$MAPE,
    medidas_ingenuo_austres_mm$MAPE
  ),
  MASE = c(
    mase_estim_austres_mm,
    mase_valid_austres_mm,
    mase_ingenuo_austres_mm
  )
)

tabla_medidas_austres_mm

# Validación de los errores
posiciones_austres_mm <- which(!is.na(modelo_austres_mm$yhat))

errores_austres_mm <- y_estimacion_austres_mm[posiciones_austres_mm] - modelo_austres_mm$yhat[posiciones_austres_mm]

resultado_errores_austres_mm <- validar_errores(
  e = errores_austres_mm,
  m = NULL,
  p = 1    # media móvil: 1 parámetro (k)
)

#Gráfico errores

resultado_errores_austres_mm$grafico_errores

#ACF, PACF
resultado_errores_austres_mm$correlograma$grafico

#Prueba t errores
cat(
  "Estadístico de prueba:", round(resultado_errores_austres_mm$t, 4), "\n",
  "Grados de libertad:", resultado_errores_austres_mm$grados_libertad_t, "\n",
  "Valor crítico:", round(
    qt(0.975, resultado_errores_austres_mm$grados_libertad_t), 4
  ), "\n",
  "Valor p:", round(resultado_errores_austres_mm$valor_p_t, 4), "\n"
)

#Ljung Box
cat(
  "Estadístico de prueba:", round(
    resultado_errores_austres_mm$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_austres_mm$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_austres_mm$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    resultado_errores_austres_mm$ljung_box$valor_p, 4
  ), "\n"
)

#Jarque Bera
cat(
  "Estadístico de prueba:", round(
    resultado_errores_austres_mm$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_austres_mm$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_austres_mm$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    resultado_errores_austres_mm$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

#Durbin Watson
cat(
  "Estadístico de prueba:", round(
    resultado_errores_austres_mm$durbin_watson$estadistico, 4
  ), "\n",
  "Grados de libertad: no aplica\n",
  "Valor crítico: no calculado\n",
  "Valor p: no calculado\n"
)

#Grafico Final
ggplot2::ggplot(
  data.frame(
    tiempo = 1:T_austres,
    real = datos_austres$y,
    ajuste = c(modelo_austres_mm$yhat, rep(NA, h_austres_mm)),
    pronostico = c(rep(NA, T_austres - h_austres_mm), pronosticos_austres_mm)
  ),
  ggplot2::aes(x = tiempo)
) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = real),
    color = "purple"
  ) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = ajuste),
    color = "green"
  ) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = pronostico),
    color = "green"
  ) +
  
  ggplot2::geom_vline(
    xintercept = T_austres - h_austres_mm,
    color = "red",
    linetype = "dashed"
  ) +
  
  ggplot2::labs(
    title = "Población australiana (contraejemplo): ajuste y pronóstico de media móvil",
    x = "Tiempo",
    y = "Población (miles de personas)"
  ) +
  
  ggplot2::theme_minimal()









