https://github.com/ispulgarind/st-2026-2-tarea1-herramientas-pulgarin-durango-isabela.git

# st-2026-2-tarea1-herramientas-pulgarin-durango-isabela

Tarea 1 - Herramientas de Series de Tiempo


## Qué contiene el repositorio

| Archivo | Contenido | Dependencias |
|---|---|---|
| `R/00-lectura.R` | `leer_serie()`: importa una serie desde un objeto `ts` o un CSV con columnas `fecha` y `valor`, detecta la frecuencia (anual, trimestral o mensual) y devuelve un tibble con atributos de fuente y unidad. | `tibble` |
| `R/01-graficos.R` | `graficar_serie()`, `correlograma()`, `panel_diagnostico()`, `graficar_optimizacion()`, `graficar_holt()`: funciones de visualización para la serie, su ACF/PACF y las superficies de optimización de parámetros. | `ggplot2`, `patchwork` |
| `R/02-metodos.R` | Los ocho métodos de pronóstico (`ajustar_media`, `ajustar_mm`, `ajustar_dmm`, `ajustar_ses`, `ajustar_holt`, `ajustar_tendencia` con tipos lineal/cuadrática/exponencial), la función auxiliar `resumen_regresion()` (mínimos cuadrados con error estándar robusto HAC) y `optimizar()` (búsqueda en rejilla por MSE de un paso). | ninguna externa (solo funciones base de R) |
| `R/03-evaluacion.R` | `medidas()` (MSE, MAD, MAPE, MASE), `ljung_box()`, `jarque_bera()`, `durbin_watson()` y `validar_errores()`, que reúne el diagnóstico completo de los residuos de un modelo. | `ggplot2` (a través de `correlograma()`) |
| `R/ejemplos.R` | Pruebas de verificación de cada función contra sus equivalentes de R base (`stats::acf`, `stats::Box.test`, `lm()`), confirmando que los cálculos manuales coinciden con los de R. | `stats` |
| `informe.qmd` | Documento principal: aplica el protocolo completo (a-e) a las ocho series elegidas y al contraejemplo. | `knitr`, `tidyverse`, `kableExtra`, `patchwork` |

## Cómo se corre

1. Clonar el repositorio y abrir el archivo `.Rproj` en RStudio.
2. Instalar las dependencias si no están presentes:

   ```r
   install.packages(c("tidyverse", "knitr", "kableExtra", "patchwork"))
   ```

3. Renderizar el informe con alguna de estas opciones:

   ```r
   quarto::quarto_render("informe.qmd")
   ```

   ```
   quarto render informe.qmd
   ```

   O usando el botón **Render** de RStudio.

Tiempo de ejecución observado: aproximadamente 8 minutos.

## Cómo se usan las funciones

Ejemplo: ajustar suavizamiento exponencial simple (SES) a una serie y obtener un pronóstico a 3 pasos.

```r
source("R/02-metodos.R")

y <- c(10, 12, 13, 15, 16, 18, 20, 21)

# Ajustar el modelo con un alpha fijo
modelo <- ajustar_ses(y, alpha = 0.3)

# Ver el ajuste dentro de la muestra (un paso adelante)
modelo$yhat

# Pronosticar 3 periodos hacia adelante
modelo$pronosticar(3)

# Ver el parámetro usado
modelo$parametros$alpha
```

Cada función de ajuste (`ajustar_media`, `ajustar_mm`, `ajustar_dmm`, `ajustar_ses`, `ajustar_holt`, `ajustar_tendencia`) devuelve una lista con `yhat` (ajuste dentro de la muestra), `pronosticar(h)` (función para pronosticar h pasos) y `parametros` (los parámetros usados y, para los métodos de tendencia, la tabla de coeficientes con error estándar robusto).

## Convenciones que fijan los números

- **Divisor de la ACF:** `correlograma()` calcula la autocorrelación con un divisor único, `sum((y - mean(y))^2)`, igual para todos los rezagos (no se usa `T - h` por rezago). Esto coincide con `stats::acf()` de R, verificado en `R/ejemplos.R` con una diferencia menor a 1e-12.
- **Media simple:** el primer pronóstico dentro de la muestra se calcula en `yhat[2] = y[1]`; a partir de ahí, cada `yhat[t+1]` es el promedio acumulado de `y[1], ..., y[t]`.
- **Media móvil (orden k):** el primer valor ajustado aparece en `yhat[k+1]`, como el promedio de las k observaciones anteriores (`y[1], ..., y[k]`); el pronóstico fuera de muestra es el promedio de las últimas k observaciones, repetido h veces.
- **Doble media móvil (orden k):** requiere al menos `2k - 1` observaciones. La primera media móvil (MM) se centra en `t = k`; la segunda (DMM) en `t = 2k - 1`. El nivel se calcula como `E = 2*MM - DMM` y la pendiente como `beta1 = (2/(k-1)) * (MM - DMM)`, ambos evaluados por primera vez en `t = 2k - 1`.
- **SES:** el nivel inicial es el primer dato de la serie (`yhat[2] = y[1]`); de ahí en adelante, `yhat[t+1] = alpha*y[t] + (1-alpha)*yhat[t]`.
- **Holt lineal:** se inicializa con `L[1] = y[1]` (nivel igual al primer dato) y `b[1] = 0` (pendiente inicial nula).
- **Tendencia lineal y cuadrática:** se ajustan por mínimos cuadrados ordinarios sobre `t = 1, ..., T`, sin necesitar inicialización ni parámetro a optimizar.
- **Tendencia exponencial:** se ajusta por mínimos cuadrados sobre `log(y)` en función de `t`; el pronóstico se calcula en escala logarítmica y se transforma con `exp()`. No incluye corrección de sesgo por defecto (`corregir_sesgo = FALSE`).
- **Error estándar robusto:** `resumen_regresion()` usa una matriz de varianza robusta tipo HAC con núcleo de Bartlett y `L = floor(4*(T/100)^(2/9))` rezagos, la regla de Newey-West.
- **Optimización de parámetros:** `optimizar()` elige el valor de la rejilla que minimiza el MSE de un paso dentro del tramo de estimación, para k (mm, dmm), alpha (ses) o la combinación alpha-beta (holt).

## Resumen de resultados

| Serie | Método | Parámetros | MASE método (val.) | MASE ingenuo (val.) |
|---|---|---|---|---|
| Descubrimientos científicos | Media simple | sin parámetros | 0.9192 | 1.1365 |
| Temperatura Nottingham | Media móvil | k=2 | 2.9626 | 0.6186* |
| Precio del Petróleo | Doble media móvil | k=2 | 0.6433 | 1.2192 |
| Descubrimientos científicos | SES | alpha=0.2 | 0.6131 | 1.1365 |
| Población australiana | Tendencia lineal | β₀=12963.29, β₁=50.71 | 3.9735 | 6.0912 |
| Millas de pasajeros | Tendencia cuadrática | β₀=1420.12, β₁=-473.93, β₂=74.33 | 1.2055 | 4.4959 |
| Johnson & Johnson | Tendencia exponencial | a=-0.6968, theta=0.0428 | 4.1401 | 9.4584 |
| Uso de Internet | Holt lineal | alpha=0.9, beta=0.9 | 1.7051 | 7.6247 |

*Referente estacional (ingenuo estacional), ya que la serie tiene ciclo de 12 meses.

**Contraejemplo:** Población australiana con media móvil (k=2) — MASE de validación 6.6502, frente a 6.0912 del ingenuo. El método no supera al referente porque la serie tiene tendencia sostenida y la media móvil asume un nivel estable, un supuesto que esta serie no cumple.


### Uso de asistentes de inteligencia artificial

Durante el desarrollo de la tarea se utilizó un asistente de inteligencia artificial como apoyo para resolver dudas puntuales. La implementación de las funciones, las pruebas y la revisión de los resultados se realizaron directamente en RStudio.

Una de las principales ayudas estuvo relacionada con **Git y GitHub**, ya que inicialmente no se tenía conocimiento sobre estas herramientas. Se recibió orientación para crear y organizar el repositorio de GitHub de acuerdo con la estructura solicitada, conectar el proyecto de RStudio con GitHub y comprender el uso de operaciones como `commit` y `push`. Se realizaron personalmente la configuración, los commits y las comprobaciones necesarias para verificar que los archivos y carpetas aparecieran correctamente en GitHub.

Durante la implementación de `leer_serie()` se consultaron dudas puntuales sobre la creación y manejo de fechas, el uso de frecuencias mensual, trimestral y anual y la conversión de un trimestre a su mes inicial. Se recibieron explicaciones sobre funciones como `paste0()`, `as.Date()` y `seq.Date()`, así como sobre la construcción del índice de meses `anio * 12 + mes` y la conversión del trimestre mediante `(inicio[2] - 1) * 3 + 1`. La función se ejecutó en RStudio con diferentes casos de prueba para comprobar los resultados.

Para `graficar_serie()` se consultaron aspectos sencillos relacionados con la presentación de los gráficos, como el uso de `theme_minimal()` y la forma de utilizar funciones de un paquete mediante la notación `paquete::función()`. También se consultó cómo utilizar `patchwork` para organizar los gráficos de ACF y PACF en un mismo panel. Estos elementos fueron probados directamente en RStudio.

En correlograma() se consultaron dudas sobre las bandas de confianza y sobre algunos aspectos de la construcción del ACF y PACF. Para las funciones de evaluación también se realizaron consultas relacionadas con las pruebas de Ljung-Box, Jarque-Bera y Durbin-Watson, principalmente para comprender qué evalúa cada una y cómo se plantean sus hipótesis. Estas orientaciones sirvieron como apoyo para la aplicación y comprensión de las pruebas, cuyos cálculos y resultados fueron obtenidos y revisados directamente en RStudio.

Durante la elaboración del informe en Quarto también se consultaron aspectos básicos de formato y organización. Entre ellos estuvieron la creación de pestañas mediante `::: {.panel-tabset}`, la forma de cargar los archivos de código desde el informe mediante `source()` para evitar ejecutarlos manualmente uno por uno, y el uso de las opciones `#| message: false` y `#| warning: false` para ocultar mensajes informativos y advertencias durante la ejecución. También se consultó cómo cargar series incluidas en R, como `AirPassengers`, y cómo utilizar `kable` y `knitr` para presentar tablas con un formato adecuado.

Además, se realizaron consultas puntuales sobre la escritura de fórmulas matemáticas en Quarto mediante `$$`, el uso de `cat()` para mostrar resultados y texto durante la ejecución y algunos aspectos relacionados con la presentación de los mapas de optimización de parámetros de Holt.

También se consultó sobre la creación de un código para guardar automáticamente los gráficos generados en la carpeta figs/. La orientación permitió utilizar funciones como ls(), get(), inherits() y ggsave() para identificar los objetos gráficos presentes en el ambiente y guardarlos como archivos .png. Este código fue ejecutado en RStudio para comprobar que las figuras se almacenaran correctamente en la carpeta correspondiente.

Finalmente, el asistente también se utilizó para aclarar la interpretación de algunos resultados estadísticos y revisar la redacción de ciertas partes del informe. Los códigos, cálculos y resultados utilizados en la entrega fueron ejecutados y verificados por cuenta propia en RStudio.



















