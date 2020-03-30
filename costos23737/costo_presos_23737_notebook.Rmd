---
title: "Costos de la Provincia de Buenos Aires de presos/as por infracción a la ley 23737 (2005-2019)"
output: html_notebook
---
Por **Lucas Pecina** lucaspecina@gmail.com

**Unidad de Planificación Estrategica** - Ministerio de Justicia - Provincia de Buenos Aires

Este trabajo fue realizado utilizando el lenguaje R. Es totalmente reproducible. Para consultas sobre los datos y otras partes del código, pueden escribir al mail señalado arriba.

```{r echo=TRUE}
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(zoo)
```

### Pre-procesamiento de datos

La fuente principal es el conjunto de datos del SNEEP (Sistema Nacional de Estadísticas sobre Ejecución de la Pena) del Ministerio de Justicia de la Nacion. El dataset esta disponible en la pagina de datos del Ministerio http://datos.jus.gob.ar/dataset/sneep con el nombre de **"Sistema Nacional de Estadísticas sobre Ejecución de la Pena - SNEEP - 2002-2018 - unificado"**. Este recurso contiene los censos del SNEEP correspondientes a los años 2002 a 2018 organizados en un archivo unificado.

El dataset contiene datos hasta el 31 de diciembre de 2018, por lo tanto, utilizare la informacion de un informe de la Unidad de Gestion y Coordinacion Estadistica del Servicio Penitenciario Bonaerense, del Ministerio de Justicia de la PBA. (detallare mas adelante)

Tal como aparece en el github del SNEEP https://github.com/datos-justicia-argentina/Sistema-Nacional-de-Estadisticas-sobre-Ejecucion-de-la-Pena-SNEEP/blob/master/Sistema-Nacional-de-Estadisticas-sobre-Ejecucion-de-la-Pena-SNEEP-metadata.md : 

"En este conjunto de datos se detallan los datos recopilados en el censo realizado sobre el total de la población detenida al día 31 de diciembre de cada año, en cada establecimiento de la República Argentina. La unidad de análisis son las personas alojadas en los establecimientos. 

El censo recaba la siguiente información sobre cada interno: edad, sexo, nacionalidad, estado civil, nivel de instrucción, situación laboral, lugar de residencia, jurisdicción judicial, situación legal, fecha de detención, fecha de condena, establecimiento de procedencia, tipo de delitos imputado, participación en trabajo remunerado, en actividades de capacitación laboral, en actividades recreativas, asistencia médica, vistas, alteraciones al orden, sanciones disciplinarias, calificaciones de conducta, tentativas de fugas o evasiones, tentativa de suicidios, lesiones recibidas, duración de la condena, medidas de seguridad, reincidencia, régimen de progresividad, salidas transitorias, régimen de semilibertad, programe de prelibertad, prisión discontinua, semidetención, reducción de pena, mujeres alojadas con sus hijos.


```{r echo=TRUE}
#sneep_unificado <- read_csv("sneep-unificado-2002-2018.csv")

head(sneep_unificado)
```


```{r echo=TRUE}
colnames(sneep_unificado)
```

Vemos que el dataset cuenta con 87 columnas. En el github https://github.com/datos-justicia-argentina/Sistema-Nacional-de-Estadisticas-sobre-Ejecucion-de-la-Pena-SNEEP/blob/master/Sistema-Nacional-de-Estadisticas-sobre-Ejecucion-de-la-Pena-SNEEP-metadata.md se encuentra el diccionario de variables. Usare estas descripciones para explicar el proceso.

Para filtrar y dejar solo los datos que nos interesan, seguiremos la siguiente secuencia de pasos:

- En primer lugar, tomaremos los individuos cuyo año de censo sea 2005 o superiores. Uso la variable **anio_censo (int)**: año en el que se realizó el censo.

- En segundo lugar, debemos dejar solo aquellos individuos cuya causa figure en la jurisdiccion de la provincia de buenos aires. Tal como dice la variable **jurisdiccion_id (int)**: código de la jurisdicción judicial interviniente en la causa penal motivo de la detención

- Luego, detectamos que hay diversos establecimientos que no son de competencia de la PBA, sino Federales y de otros lugares. Para eliminarlos del dataset, identificamos aquellos establecimientos segun su id que contengan la palabra FEDERAL, SPF o FED. Uso la variable **establecimiento_id (int)**: código del establecimiento penitenciario. 

```{r echo=TRUE}
establecimientos  <- sneep_unificado %>% filter(jurisdiccion_id==1 & anio_censo>=2005 &
   (delito1_descripcion=='Infracción ley n° 23.737 (estupefacientes)' | delito2_descripcion=='Infracción ley n° 23.737 (estupefacientes)' | 
    delito3_descripcion=='Infracción ley n° 23.737 (estupefacientes)' | delito4_descripcion=='Infracción ley n° 23.737 (estupefacientes)' |
    delito5_descripcion=='Infracción ley n° 23.737 (estupefacientes)')) %>%
  group_by(establecimiento_descripcion, establecimiento_id) %>% summarise(cantidad=n())

print(establecimientos)
```


- Por ultimo, dejamos solo los establecimientos que esten ubicados en la PBA con la variable **provincia_sneep_id (int)**: código de provincia SNEEP donde se encuentra el establecimiento penitenciario censado.

> El dataset principal que usaremos queda conformado de la siguiente manera

```{r echo=TRUE}
ley23737 <- sneep_unificado %>% filter(jurisdiccion_id==1 & provincia_sneep_id==1 & anio_censo>=2005 & 
        (delito1_descripcion=='Infracción ley n° 23.737 (estupefacientes)' | delito2_descripcion=='Infracción ley n° 23.737 (estupefacientes)' | 
         delito3_descripcion=='Infracción ley n° 23.737 (estupefacientes)' | delito4_descripcion=='Infracción ley n° 23.737 (estupefacientes)' |
         delito5_descripcion=='Infracción ley n° 23.737 (estupefacientes)') &
        !establecimiento_id %in% c(63,64,65,67,68,70,87,89,350,358,505,537) & # != 'FEDERAL'
        !establecimiento_id %in% c(66,71,72,73,74,75,76,78,84,85,86,88,90,91,92)) # != 'SPF'

head(ley23737, 10)
```


### Analisis de datos

Para tener una idea general, contamos la cantidad de presos/as por año que hubo por infracciones a la ley 23737.

```{r echo=TRUE}
cuantos_por_anio <- ley23737 %>% group_by(anio_censo) %>% summarise(presos_ley23737= n())

print(cuantos_por_anio)
```


Esto lo comparamos sobre el total de presos/as para todos los delitos. Esto lo hacemos por cada año

```{r echo=TRUE}
cantidad_delitos_poranio <- sneep_unificado %>% filter(jurisdiccion_id==1 & provincia_sneep_id==1 & anio_censo>=2005 & 
                                                         !establecimiento_id %in% c(63,64,65,67,68,70,87,89,350,358,505,537) &
                                                         !establecimiento_id %in% c(66,71,72,73,74,75,76,78,84,85,86,88,90,91,92)) %>% 
                                                         group_by(anio_censo) %>% summarise(presos_total= n())

print(cantidad_delitos_poranio)
```

Graficamos la evolucion temporal

```{r echo=TRUE}
cantidades_presos_total_ley23737 <- cuantos_por_anio %>% inner_join(cantidad_delitos_poranio, by='anio_censo') %>% gather('poblacion','cantidad_presos',-anio_censo)

ggplot(cantidades_presos_total_ley23737, aes(x = anio_censo, y = cantidad_presos, colour = poblacion)) + 
    geom_smooth() + geom_point() + scale_x_continuous("Fecha",breaks = seq(2005,2018,1)) + ggtitle("Evolución temporal de la cantidad de presos") + xlab("Año") + ylab("Cantidad presos/as")

```

Ahora es necesario cargar la informacion sobre los presupuestos destinados al sistema penitenciario de la PBA en cada año. El presupuesto se extrajo de la información brindada por la Dirección Provincial de Presupuesto Público, disponible en https://www.gba.gob.ar/hacienda_y_finanzas/direccion_provincial_de_presupuesto_publico

```{r echo=TRUE}
#presupuesto_spb <- read_excel("presupuesto_spb_por_año.xlsx")
print(presupuesto_spb)
```

```{r echo=TRUE}
#presupuesto_spb <- read_excel("presupuesto_spb_por_año.xlsx")
ggplot(presupuesto_spb, aes(x = anio, y = presupuesto)) + 
    geom_smooth() + geom_point() + scale_x_continuous("Fecha",breaks = seq(2005,2018,1)) + ggtitle("Evolución temporal del presupuesto del SPB") + xlab("Año") + ylab("Pesos argentinos")
```


El trabajo consistira de tres metodos de estimacion de los costos de presos/as por infraccion a la ley 23737.

> **Metodo 1 - Estimacion con valores al 31 de diciembre**: Usaremos los datos tal como vienen en el SNEEP para calcular la proporcion de presos por delitos por estupefacientes sobre el total de delitos (por cada año). Luego asumiremos que esta distribucion se mantiene en todo el año y calcularemos los costos actualizados por inflacion usando estos valores.

> **Metodo 2 - Estimacion con imputaciones lineales**: Se realiza una recta desde un periodo temporal al siguiente (de año a año) y se simula la cantidad de presos (totales y por ley 23737) de cada mes segun el valor de la recta: *presos_año_anterior+((presos_año_actual-presos_año_anterior)/12)*mes*. Luego, se utilizan estas cifras para calcular los costos actualizados por inflacion.

> **Metodo 3 - Estimacion con imputacion por media**: Se toma la media entre los dos puntos (cantidad presos año anterior y cantidad presos año actual). Esos valores se utilizaran para todos los meses para calcular costos y actualizar por inflacion.


### **Metodo 1 - Estimacion con valores al 31 de diciembre**

Para aproximar los costos de los presos/as por infraccion a la ley 23737, se calcula la proporcion de este subconjunto de individuos sobre la totalidad de individuos. Luego se utiliza esa proporcion sobre el presupuesto destinado en ese año para estimar el monto destinado a dicho subconjunto.

```{r echo=TRUE}
#aca calculo la proporcion de presos por droga sobre el total de presos para cada año
ley23737_sobre_total <- cuantos_por_anio %>% inner_join(cantidad_delitos_poranio, by='anio_censo')

ley23737_sobre_total_copia <- ley23737_sobre_total # este va a ser utilizado mas adelante, en el metodo 2
  
ley23737_sobre_total <- ley23737_sobre_total %>% mutate(presos_23737_relativo=presos_ley23737/presos_total)
colnames(ley23737_sobre_total) <- c('anio','presos_ley23737','presos_total','presos_23737_relativo')

#junto el resultado con el presupuesto para cada año
ley23737_sobre_total <- ley23737_sobre_total %>% inner_join(presupuesto_spb,by='anio')

print(ley23737_sobre_total)
```

Nos queda agregar los datos de 2019. Como expuse anteriormente, los datos fueron extraidos de la Dirección General de Asistencia y Tratamiento SPB / Dirección Provincial de Alcaídas Departamentales. Lo agregamos al conjunto de datos que venimos trabajando

```{r echo=TRUE}
data_2019 <- c(2019,	4915,	45366,	0.108341,	23417065600	) # estos son los datos (ordenados segun las columnas de droga_sobre_total)

ley23737_sobre_total <- rbind(ley23737_sobre_total, data_2019)

print(ley23737_sobre_total)
```


Por ultimo, debemos corregir los costos por la inflacion, para saber a cuanto equivale al valor del peso actual (marzo 2020). Tomamos los costos para los presos/as por infraccion a la ley 23737 y los actualizamos en función del Indice de Precios del Instituto de Trabajo y Economia, de la Fundacion German Abdala.

Cargo datos de inflacion
```{r echo=TRUE}
#indice_precios_consumidor <- read_excel("C:/Users/Usuario/Desktop/Lucas/1- pe/5-presos/DATA/indice_precios_consumidor.xlsx")
print(indice_precios_consumidor)
```

Se utiliza la formula acumulada: precio_anterior*(1+porcentaje_variacion_mensual) para calcularlo.

```{r echo=TRUE}
#primero hago join con anios_meses
metodo1 <- ley23737_sobre_total %>% inner_join(anios_meses %>% filter(anio>2005 | mes==12),by='anio') %>% mutate(presupuesto_mensual=presupuesto/12) %>%
  mutate(costo_presos23737_mensual= presupuesto_mensual*presos_23737_relativo)
metodo1 <- metodo1 %>% select(c('anio','mes','presos_ley23737','presos_total','presos_23737_relativo','presupuesto','presupuesto_mensual','costo_presos23737_mensual' ))

#calculo inflacion
actualizados <- c()
for(i in 1:nrow(metodo1)){
  presupuesto <- metodo1$costo_presos23737_mensual[i]
  for(j in i:(nrow(indice_precios_consumidor)-1)){
    presupuesto <- presupuesto*(1+indice_precios_consumidor$inflacion[j+1])}
  actualizados <- c(actualizados, presupuesto)}
metodo1['costo_presos23737_mensual_actualizado'] <- actualizados

print(metodo1)
```

Agrupo por años y resumo la informacion.
```{r echo=TRUE}
metodo1_agrupado <- metodo1 %>% group_by(anio) %>% summarise(presos_total_31dic=first(presos_total), 
                                                                   presos_23737_31dic=first(presos_ley23737),
                                                                   presupuesto=first(presupuesto),
                                                                   presupuesto_mensual=first(presupuesto_mensual),
                                                                   proporcion_presos23737=mean(presos_23737_relativo),
                                                                   costo_presos23737_anual=sum(costo_presos23737_mensual),
                                                                   costo_presos23737_anual_actualizado=sum(costo_presos23737_mensual_actualizado))
print(metodo1_agrupado)
```

Grafico los costos.
```{r echo=TRUE}
ggplot(metodo1_agrupado, aes(anio, costo_presos23737_anual_actualizado)) + geom_point()  + geom_text(aes(label=costo_presos23737_anual_actualizado),hjust=0, vjust=0) + geom_smooth() + scale_x_continuous(breaks=metodo1_agrupado$anio) + ggtitle("Costos estimados destinados a presos/as por infracción a la ley 23737") + xlab("Año") + ylab("Costos (presos/as por infracción a la ley 23737)")
```

**COSTO FINAL DE PRESOS/AS POR INFRACCION A LA LEY 23737 (2005/2019) - METODO 1**

Sumo todos los años para que nos de el costo total:
```{r echo=TRUE}
print(paste('Usando el Metodo 1 (valores al 31 dic), el costo total (2005-2019) de los presos/as por infraccion a la ley 23737: $', sum(metodo1$costo_presos23737_mensual)))
print(paste('Usando el Metodo 1 (valores al 31 dic), el costo total (2005-2019) de los presos/as por infraccion a la ley 23737, actualizado por inflacion es: $', sum(metodo1$costo_presos23737_mensual_actualizado)))
```


### **Metodo 2: Imputacion mensual por aproximaciones lineales**

Comenzamos con la copia del conjunto de datos que realizamos anteriormente
```{r echo=TRUE}
# ley23737_sobre_total_copia <- ley23737_sobre_total
colnames(ley23737_sobre_total_copia) <- c('anio','presos_ley23737','presos_total')

print(ley23737_sobre_total_copia)
```

Lo que queremos hacer es "simular" para cada mes de cada año cuantos presos hay en total y cuantos por infraccion a la ley 23737, de una forma que siga una linea recta que vaya del valor de un año al siguiente (de los datos analizados provenientes de SNEEP). Luego calcular las estadisticas correspondientes. 

Para eso comienzo haciendo un join con un dataset que contiene la estructura buscada

```{r echo=TRUE}
para_calculo_23737 <- c(6,6,334,1010,1462,1696,2113,2500,2534,2544,2435,2616,2642,3730)
ley23737_sobre_total_copia['cantidad_anterior_23737'] <- para_calculo_23737

para_calculo_total <- c(12788,12788,20109,21491,22769,22858,25874,26871,26780,27860,30177,31619,33461,37342)
ley23737_sobre_total_copia['cantidad_anterior_total'] <- para_calculo_total

para_2019 <- c(2019,4915,45366,4960,42255)
ley23737_sobre_total_copia <- ley23737_sobre_total_copia %>% rbind(para_2019)

metodo2_23737 <- ley23737_sobre_total_copia %>% inner_join(anios_meses, by='anio')

print(metodo2_23737)
```

Visualizo como quedaria conformado el valor de cada mes. En los siguientes graficos, cada punto es un mes particular. Estos siguien una linea recta desde un año al siguiente. Utilizaremos estas cifras como estimaciones mensuales.
```{r echo=TRUE, warning=FALSE}
# creo una variable de fecha
library(zoo)
metodo2_23737$fecha <- as.Date(as.yearmon(paste(metodo2_23737$anio,metodo2_23737$mes), '%Y %m'))

# realizo la progresion lineal y le asigno un valor sobre la recta a cada mes
## para presos_ley23737
metodo2_23737 <- metodo2_23737 %>% mutate(presos_ley23737_mensual = (cantidad_anterior_23737+((presos_ley23737-cantidad_anterior_23737)/12)*mes))
## para presos_total
metodo2_23737 <- metodo2_23737 %>% mutate(presos_total_mensual = (cantidad_anterior_total+((presos_total-cantidad_anterior_total)/12)*mes))

ggplot(metodo2_23737 %>% mutate(id = row_number()) %>% select(c('fecha','presos_ley23737_mensual','presos_total_mensual')) %>% gather('poblacion','cantidad_presos',-fecha), aes(x=fecha,y=cantidad_presos,colour=poblacion)) + geom_point() + geom_line() + ggtitle("Cantidad de presos/as por mes (simulado)")  + ylab("Cantidad de presos/as") + scale_x_date('Fecha',breaks=seq.Date(from = min(metodo2_23737$fecha), to = max(metodo2_23737$fecha), by = "1 year"), date_labels = '%Y')

```

Uso el conjunto de datos de presupuestos de spb y le agrego 2019
```{r echo=TRUE, warning=FALSE}
presupuesto_spb_2 <- presupuesto_spb
presupuesto_spb_2 <- presupuesto_spb_2 %>% rbind(c(2019,23417065600))
print(presupuesto_spb_2)
```

Calculo la proporcion de presos por ley 23737 sobre los presos totales. Dejo solo a partir de diciembre de 2005. Creo el presupuesto mensual y lo uso para obtener el costo de presos/as por infraccion a la ley 23737 mensual.
```{r echo=TRUE, warning=FALSE}
metodo2_23737 <- metodo2_23737 %>% mutate(presos_23737_relativo=presos_ley23737_mensual/presos_total_mensual) %>% inner_join(presupuesto_spb_2, by='anio')
metodo2_23737 <- metodo2_23737 %>% filter(anio > 2005 | mes==12)
metodo2_23737 <- metodo2_23737 %>% mutate(presupuesto_mensual = presupuesto/12)
metodo2_23737 <- metodo2_23737 %>% mutate(costo_presos23737_mensual= presupuesto_mensual*presos_23737_relativo)
print(metodo2_23737)
```

Grafico las distintas proporciones de presos/as por ley 23737 sobre el total de presos. La linea roja es la media de las proporciones.
```{r echo=TRUE, warning=FALSE}
ggplot(metodo2_23737, aes(x=fecha ,y=presos_23737_relativo)) + geom_point() + geom_line() + ggtitle("Proporcion de presos/as por ley 23737 sobre el total")  + ylab("Cantidad de presos/as") + scale_x_date('Fecha',breaks=seq.Date(from = min(metodo2_23737$fecha), to = max(metodo2_23737$fecha), by = "1 year"), date_labels = '%Y') + geom_hline(yintercept=mean(metodo2_23737$presos_23737_relativo), linetype="dashed", color = "red")
```

Calcular la inflacion: Utilizo la funcion anteriormente construida para estos nuevos datos.

```{r echo=TRUE}
actualizados <- c()
for(i in 1:nrow(metodo2_23737)){
  presupuesto <- metodo2_23737$costo_presos23737_mensual[i]
  for(j in i:(nrow(indice_precios_consumidor)-1)){
    presupuesto <- presupuesto*(1+indice_precios_consumidor$inflacion[j+1])}
  actualizados <- c(actualizados, presupuesto)}
```

Lo agrego como una nueva columna.
```{r echo=TRUE, warning=FALSE}
metodo2_23737['costo_presos23737_mensual_actualizado'] <- actualizados

print(metodo2_23737)
```

Agrupo por año y calculo lo necesario
```{r echo=TRUE, warning=FALSE}
metodo2_agrupado <- metodo2_23737 %>% group_by(anio) %>% summarise(presos_total_31dic=first(presos_total), 
                                                           presos_23737_31dic=first(presos_ley23737),
                                                           presos_total_media_lineal= mean(presos_total_mensual),
                                                           presos_23737_media_lineal= mean(presos_ley23737_mensual),
                                                           proporcion_presos23737_medialineal=mean(presos_23737_relativo),
                                                           presupuesto=first(presupuesto),
                                                           presupuesto_mensual=first(presupuesto_mensual), 
                                                           costo_presos23737_anual=sum(costo_presos23737_mensual),
                                                           costo_presos23737_anual_actualizado=sum(costo_presos23737_mensual_actualizado))

print(metodo2_agrupado)
```

**COSTO FINAL DE PRESOS/AS POR INFRACCION A LA LEY 23737 (2005/2019)**

Sumo todos los años para que nos de el costo total:
```{r echo=TRUE, warning=FALSE}
print(paste('Usando el Metodo 2 (aproximaciones lineales), el costo total (2005-2019) de los presos/as por infraccion a la ley 23737: ', sum(metodo2_agrupado$costo_presos23737_anual)))
print(paste('Usando el Metodo 2 (aproximaciones lineales), el costo total (2005-2019) de los presos/as por infraccion a la ley 23737, actualizado por inflacion es: ', sum(metodo2_agrupado$costo_presos23737_anual_actualizado)))
```

### Metodo 3 - **Imputacion por media**

Para este caso, imputo la cantidad de presos por mes segun la media entre el valor del 31 de diciembre del año en tratamiento con el valor del 31 de diciembre del año anterior: (año_anterior+año_actual)/2

```{r echo=TRUE}
metodo3 <- metodo2_23737 %>% group_by(anio) %>% mutate(presos_total_promedio= (presos_total+cantidad_anterior_total)/2,
                                                       presos_23737_promedio= (presos_ley23737+cantidad_anterior_23737)/2)
metodo3 <- metodo3 %>% mutate(presos_23737_relativo_promedio= presos_23737_promedio/presos_total_promedio) %>% 
  mutate(costo_presos23737_mensual_promedio= presupuesto_mensual*presos_23737_relativo_promedio)
print(metodo3)
```

Actualizo por inflacion y agrupo por año
```{r echo=TRUE}
actualizados <- c()
for(i in 1:nrow(metodo3)){
  presupuesto <- metodo3$costo_presos23737_mensual_promedio[i]
  for(j in i:(nrow(indice_precios_consumidor)-1)){
    presupuesto <- presupuesto*(1+indice_precios_consumidor$inflacion[j+1])
  }
  actualizados <- c(actualizados, presupuesto)
}
metodo3['costo_presos23737_mensual_promedio_actualizado'] <- actualizados 

metodo3_agrupado <- metodo3 %>% group_by(anio) %>% summarise(costo_metodo3 = sum(costo_presos23737_mensual_promedio_actualizado))

print(metodo3_agrupado)
```

**COSTO FINAL DE PRESOS/AS POR INFRACCION A LA LEY 23737 (2005/2019)**

Sumo todos los años para que nos de el costo total:
```{r echo=TRUE}
sum(metodo3$costo_presos23737_mensual_promedio_actualizado)
```



## Diagnostico

Por ultimo vemos una tabla comparativa para cada año segun los tres metodos utilizados
```{r echo=TRUE}
comparacion_metodos <- as.data.frame(ley23737_sobre_total$anio %>% 
  cbind(metodo1 %>% group_by(anio) %>% summarise(metodo1= sum(costo_presos23737_mensual_actualizado)) %>% select(metodo1)) %>%
  cbind(metodo2_23737 %>% group_by(anio) %>% summarise(metodo2_23737= sum(costo_presos23737_mensual_actualizado)) %>% select(metodo2_23737)) %>%
  cbind(metodo3 %>% group_by(anio) %>% summarise(metodo3= sum(costo_presos23737_mensual_promedio_actualizado)) %>% select(metodo3)))
colnames(comparacion_metodos) <- c('anio','metodo1_31dic','metodo2_lineal','metodo3_promedio')

print(comparacion_metodos)
```

Comparo graficamente el metodo 1 y 2
```{r echo=TRUE}
ggplot(comparacion_metodos %>% select(-metodo3_promedio) %>% gather('metodo','costo_presos23737_actualizado',-anio), aes(x = anio, y = costo_presos23737_actualizado)) +
    geom_line(aes(colour = metodo), size = 2)+
    geom_point(colour = 'royalblue', size = 3)+
    scale_x_continuous(name="Fecha", breaks=seq(2005,2019,1)) +
    scale_y_continuous(name="Costos presos/as ley 23737") + 
    ggtitle("Comparacion de costos metodo 1 y 2")+
    theme_bw()
```

El primer metodo se diferencia un poco mas de los otros dos, sobre todo en los primeros años. Sin embargo, cuando vemos el total acumulado, estos tres metodos tienen una distancia maxima que no llega al 4% de diferencia, lo que demuestra la robustez del modelo.
```{r echo=TRUE}
print(paste('Costos de presos/as por infraccion a la ley 23737 actualizado segun METODO 1 (imputacion al 31 dic): $',sum(comparacion_metodos$metodo1_31dic)))
print(paste('Costos de presos/as por infraccion a la ley 23737 actualizado segun METODO 2 (imputacion lineal por mes): $',sum(comparacion_metodos$metodo2_lineal)))
print(paste('Costos de presos/as por infraccion a la ley 23737 actualizado segun METODO 3 (imputacion por promedio): $',sum(comparacion_metodos$metodo3_promedio)))
```







