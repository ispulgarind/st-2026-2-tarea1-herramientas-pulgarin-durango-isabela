# st-2026-2-tarea1-herramientas-pulgarin-durango-isabela
Tarea 1 - Herramientas de Series de Tiempo

### Uso de asistentes de inteligencia artificial

Durante el desarrollo de la tarea se utilizó un asistente de inteligencia artificial como apoyo para resolver dudas puntuales. La ayuda recibida se organizó de la siguiente manera:

#### 1. GitHub y organización del proyecto

**Lo que se pidió:** orientación para crear y organizar el repositorio de GitHub de acuerdo con la estructura solicitada, conectar el proyecto de RStudio con GitHub y comprender el uso de operaciones como `commit` y `push`.

**Lo que se recibió:** explicaciones sobre la estructura de carpetas y archivos, el funcionamiento de Git y GitHub y los pasos para guardar los cambios localmente y enviarlos al repositorio.

**Lo que se verificó por cuenta propia:** se realizó la configuración en RStudio, se creó el repositorio, se hicieron commits y se comprobó que los archivos y carpetas aparecieran correctamente en GitHub.

#### 2. Función `leer_serie()`

**Lo que se pidió:** orientación para implementar la función `leer_serie()` y resolver dudas puntuales sobre la creación y manejo de fechas, el uso de frecuencias mensual, trimestral y anual, la conversión de un trimestre a su mes inicial.

**Lo que se recibió:** explicaciones sobre el uso de funciones como `paste0()`, `as.Date()` y `seq.Date()`, así como sobre la construcción del índice de meses `anio * 12 + mes` y la conversión del trimestre mediante `(inicio[2] - 1) * 3 + 1`.

**Lo que se verificó por cuenta propia:** se ejecutó la función en RStudio utilizando diferentes casos de prueba y se comprobaron los resultados para series anuales, trimestrales y mensuales, además de casos con fechas faltantes, fechas no crecientes y fechas no equiespaciadas. También se revisó el funcionamiento de cada parte de la función para poder explicar su lógica.

#### 3. Función `graficar_serie()`

**Lo que se pidió:** orientación para completar la función de gráficos y presentar la información de la serie de forma clara.

**Lo que se recibió:** explicación sobre el uso de `theme_minimal()` para mejorar la presentación del gráfico y sobre cómo utilizar una función de un paquete sin llamar previamente a `library()`, mediante la notación `paquete::función()`.

**Lo que se verificó por cuenta propia:** se ejecutó la función con una serie de prueba y se comprobó que el gráfico mostrara correctamente las fechas, los valores, el título, la unidad, la fuente y el número de observaciones.

#### 4. Función `correlograma()`

**Lo que se recibió:** explicación sobre las bandas de confianza del correlograma y su relación con la hipótesis de ruido blanco.

**Lo que se verificó por cuenta propia:** se comprobó que la banda calculada coincidiera con la utilizada por R.

