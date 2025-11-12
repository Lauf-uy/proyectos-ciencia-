## Lauf.uy001
# Explorando diferencias salariales entre analistas de datos y cientificos de datos
En un mundo impulsado por los datos, la capacidad de aplicar técnicas estadísticas avanzadas para analizar e interpretar complejidades es más valiosa que nunca.

Este proyecto busca mostrar cómo se pueden aplicar pruebas estadísticas para investigar datos y cómo usar un enfoque metodológico riguroso para aumentar la confianza en los resultados.

## Sobre los Datos
Los datos utilizas corresponden a los datos Salariales de analistas y cientificos de datos de xxx paises durante los años 2020 y 2023. Fueron obtenidos en Kaggle.com

### ver código
#Se importaron las librerias de R necesarias
library(readr)
library(readxl)
library(scales)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(gt)
library(here)

Además de la preparacipón del entorno de R, en donde realizamos el estudio, procedimos a la correspondiente limpieza de registros nulos y duplicados.

### ver código
#importar dataset
datossalarios <- read.csv(here("datos/jobs_in_data.csv"))
view(datossalarios)

### ver código
#Limpieza del dataset
datossalarios <- datossalarios %>%
  na.omit() %>%          # elimina filas con cualquier NA
  distinct()             # elimina registros duplicados

## Análisis Emploratorio
Inicialmente, realizamos un análisis de los datos para comprender las características generales de los datos. Esta fase incluyó filtrar los registros relevantes para los cargos de Analista de Datos y Data Scientist, y calcular medidas descriptivas como medias, medianas y desviaciones estándar. Se crearon visualizaciones, como histogramas y boxplots para visualizar la distribución salarial e identificar posibles valores atípicos o asimetrías.


### ver código
Distribución de salarios en USD:

#Calcular media y desviación estándar
media <- mean(datossalarios$salary_in_usd, na.rm = TRUE)
desv <- sd(datossalarios$salary_in_usd, na.rm = TRUE)

#Graficar distribución con frecuencia
ggplot(datossalarios, aes(x = salary_in_usd)) +
  geom_histogram(aes(y = ..count..),
                 fill = "violet", color = "black", bins = 30, alpha = 0.6) +
  stat_function(
    fun = function(x) {
      dnorm(x, mean = media, sd = desv) * 
        diff(range(datossalarios$salary_in_usd)) / 30 * nrow(datossalarios)
    },
    color = "darkviolet", size = 1.2
  ) +
  scale_x_continuous(
    labels = label_number(scale_cut = cut_short_scale())  # eje X 100k, 200k, etc.
  ) +
  labs(
    title = "",
    x = "Salary in k-USD",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 14)



El gráfico muestra una distribución de salarios con una mayor concentración en el rango entre USD50,000 y USD200,000; aunque tiene una larga cola que se extiende hasta los salarios más altos, puede considerarse distribucion normal




Salarios según nivel de experiencia:

### ver código
#ordenar las categorías por la media de salario (de menor a mayor)
datossalarios <- datossalarios %>%
  group_by(experience_level) %>%
  mutate(media_salario = mean(salary_in_usd, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(experience_level = reorder(experience_level, media_salario))

#Graficar boxplot
ggplot(datossalarios, aes(x = experience_level, y = salary_in_usd, fill = experience_level)) +
  geom_boxplot(alpha = 0.7, outlier.color = "darkviolet") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +  # eje Y en formato 100k, 200k, etc.
  labs(
    title = "Distribución de salarios por nivel de experiencia",
    x = "Nivel de experiencia",
    y = "Salario en USD"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")  # quita la leyenda redundante



Los salarios aumentan con la experiencia, y los profesionales sénior aparentemente ganan considerablemente más que los profesionales de nivel inicial e intermedio. Existe una mayor variación en los salarios de los profesionales sénior, lo que indica una amplia gama de salarios dentro de este grupo.

.

Número de empleos por categoría:

La mayoría de los puestos se concentran en unas pocas categorías, siendo “Ciencia de Datos e Investigación” e “Ingeniería de Datos” las que cuentan con mayor número de empleos. Otro dato interesante es que el rol de Analista de Datos cuenta con más profesionales que el de Inteligencia de Negocios y Visualización.

### ver código
#Agrupar y contar empleos por categoría
conteo_jobs <- datossalarios %>%
  group_by(job_category) %>%
  summarise(cantidad = n()) %>%
  arrange(cantidad)  # para que se ordenen de menor a mayor en el eje Y

#Gráfico 
ggplot(conteo_jobs, aes(x = cantidad, y = reorder(job_category, cantidad), fill = job_category)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "black") +
  scale_fill_manual(values = c("#AAF40B", "green", "#00BB72", "#FFD500", "orange", "yellow",  "darkviolet" ,"violet", "#0BF4DC", "#27B0F5")) +
  labs(
    title = "Número de empleos por categoría",
    x = "Cantidad de empleos",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )



La mayoría de los puestos se concentran en unas pocas categorías, siendo “Ciencia de Datos e Investigación” e “Ingeniería de Datos” las que cuentan con mayor número de empleos. Otro dato interesante es que el rol de Analista de Datos cuenta con más profesionales que el de Inteligencia de Negocios y Visualización.

## Sobre los salarios de los Data Analyst y los Data Science: Foco estadístico comparativo


### ver código
#Filtrar solo Data Analyst y Data Scientist
promedio_salarios <- datossalarios %>%
  filter(job_title %in% c("Data Analyst", "Data Scientist")) %>%
  group_by(job_title) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE))

#Gráfico de barras
ggplot(promedio_salarios, aes(x = job_title, y = avg_salary, fill = job_title)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "black") +
  scale_y_continuous(
    labels = function(x) paste0(round(x / 1000), "k")  # eje Y en miles de USD
  ) +
  scale_fill_manual(values = c("darkviolet", "gold")) +  # colores diferenciados
  labs(
    title = "Salario de DataAnayst VS. Salario promedio de DataScientist",
    subtitle = "Promedio (USD) - Average",
    x = "Título de trabajo",
    y = "Salario promedio (k USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )



El Salario Pomedio en USD es entonces:

Data Analyst.: USD108,843.79

Data Scientist. USD150,533.85

Esto indica que, en promedio, los científicos de datos ganan más que los analistas de datos. Esta diferencia puede reflejar variaciones en los niveles de responsabilidad, las habilidades técnicas requeridas y la demanda del mercado para estos puestos. Sin embargo, lo que queremos analizar es si esta diferencia es estadísticamente significativa.

Para ello, se utilizó una prueba de hipótesis para comparar si los promedios son estadísticamente diferentes.

Antes de definir qué prueba estadística realizar, es necesario visualizar los datos descriptivos y definir las hipótesis:

### ver código

#Tabla descriptiva filtrada
tabla_resumen <- datossalarios %>%
  filter(job_title %in% c("Data Analyst", "Data Scientist")) %>%
  group_by(job_title) %>%
  summarise(
    N = n(),
    Media = mean(salary_in_usd, na.rm = TRUE),
    Desvio_Estandar = sd(salary_in_usd, na.rm = TRUE)
  ) %>%
  arrange(job_title)

#Mostrar tabla
tabla_resumen

#A tibble: 2 × 4
  job_title          N   Media Desvio_Estandar
  <chr>          <int>   <dbl>           <dbl>
1 Data Analyst     744 108844.          48283.
2 Data Scientist  1039 150534.          62245.
Tenemos 1388 Data Analyst y 1989 Data Scientist.



Al igual que con la diferencia en los salarios promedio entre ambos grupos observamos que la variación en estos datos parece ser diferente entre los grupos. Esto es importante porque influye directamente en la prueba de hipótesis que realizaremos.

Definicición de la hipótesis

La hipótesis nula, H0, es que el salario del Data Analyst no es menor que el del Data Scientist. La hipótesis alternativa, H1 o Ha, es que el salario promedio del Data Scientist es mayor que el salario promedio del Data Analyst .

Definición de la prueba:

Para comparar dos medias no pareadas, utilizamos la prueba t, si las muestras tienen varianza homogénea y siguen una distribución normal. Probemos, pues, estos supuestos.



Aquí vemos que, según la Prueba de Levene, los grupos no tienen varianzas homogéneas. Por lo tanto, los datos no cumplen con los supuestos necesarios para una prueba t, que consiste precisamente en la homogeneidad de las varianzas de ambos grupos.

La segunda prueba que realizamos consiste en analizar si las variables siguen una distribución normal.

Para ello, utilizamos la prueba de Shapiro-Wilk porque tenemos más de 50 valores para cada variable.

Test of Normality	Variable	Statistic	P
Shapiro-Wilk	Salay_in_usd	0.979	<.001
En la prueba obtuvimos que valor P fue menor que 0,05, lo que significa que no podemos afirmar que estas variables tengan una distribución normal.

Sin embargo, dado que tenemos variables con muchos valores, podemos utilizar el Teorema del Límite Central y considerar distribuciones normales para los datos. Al hacerlo, es recomendable analizar el gráfico de frecuencias de ambas variables.

### ver código
#Filtrar solo las dos categorías
datos_filtrados <- datossalarios %>%
  filter(job_title %in% c("Data Analyst", "Data Scientist"))

ggplot(datos_filtrados, aes(x = salary_in_usd, fill = job_title)) +
  geom_density(alpha = 0.6, color = "black") +
  facet_wrap(~ job_title, scales = "fixed") +
  scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_fill_manual(values = c("darkviolet", "gold")) +
  labs(
    title = "Distribución de salarios por título de trabajo",
    x = "Salario en USD",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")



### ver código
#Boxplot comparativo
ggplot(datos_filtrados, aes(x = job_title, y = salary_in_usd, fill = job_title)) +
  geom_boxplot(alpha = 0.7, outlier.color = "blue", color = "black") +
  scale_y_continuous(
    labels = function(x) paste0(round(x / 1000), "k")  # eje Y en miles de USD
  ) +
  scale_fill_manual(values = c("darkviolet", "gold")) +
  labs(
    title = "Distribución de salarios por título de trabajo",
    x = "",
    y = "Salario USD"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12)
  )



Aquí observamos que las distribuciones no presentan una asimetría significativa, lo que nos permite considerarlas con mayor seguridad como normales.

Para estos análisis utilizaremos la prueba t de Welch, que se emplea en análisis con varianzas heterogéneas pero con una distribución normal de los datos.

Test t de Welch	Variable	Statistic	gl	p
Para muestras independientes	Salay_in_usd	-26.4	3372	<.001

Observamos que el valor p indica que existe una diferencia significativa en los salarios promedio. Esto se debe a que es menor que 0.05. Por lo tanto, podemos rechazar la hipótesis nula de que el salario promedio de los analistas de datos no es inferior al de los científicos de datos.

## Conclusión:
Según nuestros resultados al rechazar la hipótesis nula, se puede concluir con confianza que:

Los Data Scientists, en promedio, ganan significativamente más que los Data Analysts.

Si bien puede haber variaciones dentro de cada grupo, los datos muestran de forma clara y consistente que el salario medio de los Data Scientists es más alto que el de los Data Analysts, y esta diferencia es estadísticamente significativa.

Este estudio ejemplifica la importancia de las técnicas estadísticas para validar las diferencias observadas en cualquier conjunto de datos. Elegir la prueba estadística adecuada, según las características de los datos, es fundamental para obtener conclusiones precisas y correctas.

