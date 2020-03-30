library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(openxlsx)
library(lubridate)



sneep_unificado <- read_csv("sneep-unificado-2002-2018.csv")

########## USANDO JURISDICCION ID

#agrupo por establecimiento de jurisdiccion PBA y año >=2005
establecimientos  <- sneep_unificado %>% filter(jurisdiccion_id==1 & anio_censo>=2005 &
   (delito1_descripcion=='Infracción ley n° 23.737 (estupefacientes)' | delito2_descripcion=='Infracción ley n° 23.737 (estupefacientes)' | 
    delito3_descripcion=='Infracción ley n° 23.737 (estupefacientes)' | delito4_descripcion=='Infracción ley n° 23.737 (estupefacientes)' |
    delito5_descripcion=='Infracción ley n° 23.737 (estupefacientes)')) %>%
  group_by(establecimiento_descripcion, establecimiento_id) %>% summarise(cantidad=n())

#ver los de jurisdiccion de PBA pero que aparecen en establecimientos federales (aclarar en informe)
#criterio filtro: contiene FEDERAL, contiene SPF, establecimiento esta afuera de PBA (en provincia_id)
droga_PB_estab_federales <- sneep_unificado %>% filter(jurisdiccion_id==1 & anio_censo>=2005 &
                                                                             (delito1_descripcion=='Infracción ley n° 23.737 (estupefacientes)' | delito2_descripcion=='Infracción ley n° 23.737 (estupefacientes)' | 
                                                                                delito3_descripcion=='Infracción ley n° 23.737 (estupefacientes)' | delito4_descripcion=='Infracción ley n° 23.737 (estupefacientes)' |
                                                                                delito5_descripcion=='Infracción ley n° 23.737 (estupefacientes)') &
                                                                             (establecimiento_id %in% c(63,64,65,67,68,70,87,89,350,358,505,537) | #establecim != federal
                                                         establecimiento_id %in% c(66,71,72,73,74,75,76,78,84,85,86,88,90,91,92) | # establecim != spf
                                                         provincia_sneep_id != 1))




########## EMPEZAR CORRIENDO DESDE ACA(ANTES SOLO CARGAR EL UNIFICADO)


ley23737 <- sneep_unificado %>% filter(jurisdiccion_id==1 & provincia_sneep_id==1 & anio_censo>=2005 & 
        (delito1_descripcion=='Infracción ley n° 23.737 (estupefacientes)' | delito2_descripcion=='Infracción ley n° 23.737 (estupefacientes)' | 
         delito3_descripcion=='Infracción ley n° 23.737 (estupefacientes)' | delito4_descripcion=='Infracción ley n° 23.737 (estupefacientes)' |
         delito5_descripcion=='Infracción ley n° 23.737 (estupefacientes)') &
        !establecimiento_id %in% c(63,64,65,67,68,70,87,89,350,358,505,537) & #establecim != federal
        !establecimiento_id %in% c(66,71,72,73,74,75,76,78,84,85,86,88,90,91,92)) #establecim != spf


cantidad_delitos_poranio <- sneep_unificado %>% filter(jurisdiccion_id==1 & provincia_sneep_id==1 & anio_censo>=2005 & 
                                                         !establecimiento_id %in% c(63,64,65,67,68,70,87,89,350,358,505,537) &
                                                         !establecimiento_id %in% c(66,71,72,73,74,75,76,78,84,85,86,88,90,91,92)) %>% 
                                                         group_by(anio_censo) %>% summarise(presos_total= n())



cuantos_por_anio <- ley23737 %>% group_by(anio_censo) %>% summarise(presos_ley23737= n())
ggplot(cuantos_por_anio, aes(anio_censo, presos_ley23737)) + geom_line() + geom_point()



######################## METODO 1 (corte al 31 dic) #####################

###### ahora necesito los datos de los presupuestos para calcular cuanto es el gasto total por año 
# despues para cada año actualizar el valor por inflacion y sumarlos

presupuesto_spb <- read_excel("C:/Users/Usuario/Desktop/Lucas/1- pe/5-presos/DATA/presupuesto_spb_anual.xlsx")
presupuesto_spb <- read_excel('/Users/lucaspecina/Desktop/Data/Work/PE/presos/data/presupuesto_spb_anual.xlsx')

#aca calculo la proporcion de delitos por droga sobre el total de delitos para cada año
ley23737_sobre_total <- cuantos_por_año %>% inner_join(cantidad_delitos_poranio, by='anio_censo')
colnames(ley23737_sobre_total) <- c('anio_censo','presos_ley23737','presos_total')

### PARA USAR EN METODO 2
ley23737_sobre_total_copia <- ley23737_sobre_total # -> para usar en METODO 2
ley23737_sobre_total_copia2 <- ley23737_sobre_total
###

ley23737_sobre_total <- ley23737_sobre_total %>% mutate(presos_23737_relativo=presos_ley23737/presos_total)
colnames(ley23737_sobre_total) <- c('anio','presos_ley23737','presos_total','presos_23737_relativo')

#junto este con el presupuesto para cada año
ley23737_sobre_total <- ley23737_sobre_total %>% inner_join(presupuesto_spb,by='anio')
ley23737_sobre_total <- ley23737_sobre_total %>% mutate(presupuesto_23737= presupuesto*presos_23737_relativo)


###### ahora sumar 2019 -> 
# sacado del rud 2019 y calculado con el promedio de las diferencias relativas entre rud y sneep
data_2019 <- c(2019,	4915,	45366,	0.108341,	23417065600, 2537028304	) # datos sacados de SERVICIO PENITENCIARIO BONAERENSE | UNIDAD DE GESTION Y COORDINACIÓN ESTADÍSTICA

ley23737_sobre_total <- rbind(ley23737_sobre_total, data_2019)

ggplot(ley23737_sobre_total, aes(anio, presos_ley23737)) + geom_point()  + geom_text(aes(label=presos_ley23737),hjust=0, vjust=0) + 
  geom_smooth() + scale_x_continuous(breaks=ley23737_sobre_total$anio) + 
  ggtitle("Cantidad de presos/as por infracción a la ley 23737") + xlab("Año") + ylab("Cantidad presos/as por infraccion a la ley 23737")

# le agrego la variabel de costo (ya corregido por inflacion en un excel "Costo preso")
ley23737_sobre_total['costo_23737_actualizado'] <- c(620521,	293025514,	1049198333,	1331698394,	1590981370,	1768591822,	2172346037,	2413024101,	2471304479,	2059110717,	2575693129,	2778888074,	3619223005,	3801600713,	3210741501)

View(ley23737_sobre_total)

sum(ley23737_sobre_total$costo_23737_actualizado) # 31136047710
######### final


ggplot(ley23737_sobre_total, aes(anio, costo_23737_actualizado)) + geom_point()  + geom_text(aes(label=costo_23737_actualizado),hjust=0, vjust=0) + geom_smooth() + scale_x_continuous(breaks=ley23737_sobre_total$anio)

openxlsx::write.xlsx(ley23737_sobre_total, 'C:/Users/Usuario/Desktop/Lucas/1- pe/5-presos/DATA/ley23737_sobre_total-1.xlsx')

##########################################





################## METODO 2 -> CRECIMIENTO PROGRESIVO LINEAL  #######################################

colnames(ley23737_sobre_total_copia) <- c('anio','presos_ley23737','presos_total')

anios_meses <- read_excel("C:/Users/Usuario/Desktop/Lucas/1- pe/5-presos/DATA/anios-meses.xlsx") 
anios_meses <- read_excel("/Users/lucaspecina/Desktop/Data/Work/PE/presos/data/anio-meses.xlsx")

# si es necesario
colnames(anios_meses) <- c('mes','anio')
anios_meses <- anios_meses %>% filter(mes!='mes')
anios_meses['anio'] <- as.numeric(anios_meses$anio)
anios_meses['mes'] <- as.numeric(anios_meses$mes)

para_calculo_23737 <- c(6,6,334,1010,1462,1696,2113,2500,2534,2544,2435,2616,2642,3730)
ley23737_sobre_total_copia['cantidad_anterior_23737'] <- para_calculo_23737
para_calculo_total <- c(12788,12788,20109,21491,22769,22858,25874,26871,26780,27860,30177,31619,33461,37342)
ley23737_sobre_total_copia['cantidad_anterior_total'] <- para_calculo_total

para_2019 <- c(2019,4915,45366,4960,42255)
ley23737_sobre_total_copia <- ley23737_sobre_total_copia %>% rbind(para_2019)

metodo2_23737 <- ley23737_sobre_total_copia %>% inner_join(anios_meses, by='anio')

# para delitos_droga
metodo2_23737 <- metodo2_23737 %>% mutate(presos_ley23737_mensual = (cantidad_anterior_23737+((presos_ley23737-cantidad_anterior_23737)/12)*mes))
library(zoo)
metodo2_23737$fecha <- as.Date(as.yearmon(paste(metodo2_23737$anio,metodo2_23737$mes), '%Y %m'))

ggplot(metodo2_23737, aes(fecha,presos_ley23737_mensual)) + geom_point() + geom_line()
# para delitos_total
metodo2_23737 <- metodo2_23737 %>% mutate(presos_total_mensual = (cantidad_anterior_total+((presos_total-cantidad_anterior_total)/12)*mes))
ggplot(metodo2_23737, aes(fecha,presos_total_mensual)) + geom_point() + geom_line() 

# DE NUEVO EL PRESUPUESTO (CARGO DESDE 0)
presupuesto_spb_2 <- read_excel("C:/Users/Usuario/Desktop/Lucas/1- pe/5-presos/DATA/presupuesto_spb_anual.xlsx")
presupuesto_spb_2 <- presupuesto_spb
presupuesto_spb_2 <- presupuesto_spb_2 %>% rbind(c(2019,23417065600))

metodo2_23737 <- metodo2_23737 %>% mutate(presos_23737_relativo=presos_ley23737_mensual/presos_total_mensual) %>% inner_join(presupuesto_spb_2, by='anio')
metodo2_23737 <- metodo2_23737 %>% filter(anio > 2005 | mes>11)
metodo2_23737 <- metodo2_23737 %>% mutate(presupuesto_mensual = presupuesto/12)
metodo2_23737 <- metodo2_23737 %>% mutate(costo_presos23737_mensual= presupuesto_mensual*presos_23737_relativo)

openxlsx::write.xlsx(metodo2_23737, '/Users/lucaspecina/Desktop/Data/Work/PE/presos/data/presos_23737_mensual.xlsx')
write.csv(metodo2_23737, '/Users/lucaspecina/Desktop/Data/Work/PE/presos/data/metodo2_23737.csv')


# cargue costo_preso_2
costo_preso_2 <- read_excel("/Users/lucaspecina/Desktop/Data/Work/PE/presos/data/costo_preso_2.xlsx", col_types = c("skip", "numeric", "numeric", "numeric"))
costo_preso_2 <- read_excel("C:/Users/Usuario/Desktop/Lucas/1- pe/5-presos/DATA/costo_preso_2.xlsx", col_types = c("skip", "numeric", "numeric", "numeric"))

actualizados <- c()
for(i in 1:nrow(metodo2_23737)){
  presupuesto <- metodo2_23737$costo_presos23737_mensual[i]
  for(j in i:(nrow(costo_preso_2)-1)){
    presupuesto <- presupuesto*(1+costo_preso_2$inflacion[j+1])
  }
  actualizados <- c(actualizados, presupuesto)
}

metodo2_23737['costo_presos23737_mensual_actualizado'] <- actualizados

metodo2_agrupado <- metodo2_23737 %>% group_by(anio) %>% summarise(presos_total_31dic=first(presos_total), 
                                                           presos_23737_31dic=first(presos_ley23737),
                                                           presos_total_media_lineal= mean(presos_total_mensual),
                                                           presos_23737_media_lineal= mean(presos_ley23737_mensual),
                                                           proporcion_presos23737_medialineal=mean(presos_23737_relativo),
                                                           presupuesto=first(presupuesto),
                                                           presupuesto_mensual=first(presupuesto_mensual), 
                                                           costo_presos23737_anual=sum(costo_presos23737_mensual),
                                                           costo_presos23737_anual_actualizado=sum(costo_presos23737_mensual_actualizado))
sum(metodo2_23737$costo_presos23737_mensual_actualizado) # 29943410032
sum(metodo2_agrupado$costo_presos23737_anual_actualizado) # 29943410032
### final

openxlsx::write.xlsx(metodo2_agrupado, 'C:/Users/Usuario/Desktop/Lucas/1- pe/5-presos/DATA/costospresos23737_imputacionlineal3.xlsx')



############### METODO 3 ####################
###### ver si era lo mismo sacar la media para cada año, usarla con todos los meses de inflacion y despues promediarlos.
#agrupar por año metodo2 y hacer mutate (no dejar solo años) 
metodo3 <- metodo2_23737 %>% group_by(anio) %>% mutate(presos_total_promedio= (presos_total+cantidad_anterior_total)/2,
                                                       presos_23737_promedio= (presos_ley23737+cantidad_anterior_23737)/2)
metodo3 <- metodo3 %>% mutate(presos_23737_relativo_promedio= presos_23737_promedio/presos_total_promedio) %>% 
  mutate(costo_presos23737_mensual_promedio= presupuesto_mensual*presos_23737_relativo_promedio)




actualizados <- c()
for(i in 1:nrow(metodo3)){
  presupuesto <- metodo3$costo_presos23737_mensual_promedio[i]
  for(j in i:(nrow(costo_preso_2)-1)){
    presupuesto <- presupuesto*(1+costo_preso_2$inflacion[j+1])
  }
  actualizados <- c(actualizados, presupuesto)
}
metodo3['costo_presos23737_mensual_promedio_actualizado'] <- actualizados 

sum(metodo3$costo_presos23737_mensual_promedio_actualizado) # 29903455132

(29903455132-29943410032)/29943410032 # = -0.001374366

metodo3_agrupado <- metodo3 %>% group_by(anio) %>% summarise(presos_total_31dic=first(presos_total), 
                                                                   presos_23737_31dic=first(presos_ley23737),
                                                                   presupuesto=first(presupuesto),
                                                                   presupuesto_mensual=first(presupuesto_mensual),
                                                                   presos_total_promedio= first(presos_total_promedio),
                                                                   presos_23737_promedio= first(presos_23737_promedio),
                                                                   proporcion_presos23737_promedio=mean(presos_23737_relativo_promedio),
                                                                   costo_presos23737_anual=sum(costo_presos23737_mensual_promedio),
                                                                   costo_presos23737_anual_actualizado=sum(costo_presos23737_mensual_promedio_actualizado))



openxlsx::write.xlsx(metodo3_agrupado, 'C:/Users/Usuario/Desktop/Lucas/1- pe/5-presos/DATA/metodo3_agrupado.xlsx')


###########################################################
# comparacion anual
comparacion_anios <- ley23737_sobre_total$anio %>% cbind(ley23737_sobre_total$costo_23737_actualizado) %>% 
  cbind(metodo2_agrupado$costo_presos23737_anual_actualizado) %>% cbind(metodo3_agrupado$costo_metodo3)
colnames(comparacion_anios) <- c('anio','metodo1_31dic','metodo2_lineal','metodo3_promedio')
View(comparacion_anios)


###### NUEVA INFLACION 
indice_precios_consumidor <- read_excel("C:/Users/Usuario/Desktop/Lucas/1- pe/5-presos/DATA/indice_precios_consumidor.xlsx") 
indice_precios_consumidor <- read_excel('/Users/lucaspecina/Desktop/Data/Work/PE/presos/data/indice_precios_consumidor.xlsx')

#METODO1
metodo1_nueva_inflacion <- metodo3 %>% select(c('anio','mes','presos_ley23737','presos_total','presupuesto','presupuesto_mensual')) %>% 
  mutate(proporcion_23737= presos_ley23737/presos_total) %>% mutate(costo_presos23737_mensual= presupuesto_mensual*proporcion_23737)
actualizados <- c()
for(i in 1:nrow(metodo1_nueva_inflacion)){
  presupuesto <- metodo1_nueva_inflacion$costo_presos23737_mensual[i]
  for(j in i:(nrow(indice_precios_consumidor)-1)){
    presupuesto <- presupuesto*(1+indice_precios_consumidor$inflacion[j+1])}
  actualizados <- c(actualizados, presupuesto)}
metodo1_nueva_inflacion['costo_presos23737_mensual_actualizado'] <- actualizados
sum(metodo1_nueva_inflacion$costo_presos23737_mensual_actualizado) # 29943063479

ggplot(metodo1_agrupado, aes(anio, costo_presos23737_anual_actualizado)) + geom_point()  + 
  geom_text(aes(label=round(costo_presos23737_anual_actualizado)),hjust=0, vjust=0) + geom_smooth() + 
  scale_x_continuous(breaks=seq(2005,2020,1)) + ggtitle("Costos estimados destinados a presos/as por infracción a la ley 23737") + 
  xlab("Año") + ylab("Costos (presos/as por infracción a la ley 23737)")


#METODO2
metodo2_nueva_inflacion <- metodo2_23737
actualizados <- c()
for(i in 1:nrow(metodo2_nueva_inflacion)){
  presupuesto <- metodo2_nueva_inflacion$costo_presos23737_mensual[i]
  for(j in i:(nrow(indice_precios_consumidor)-1)){
    presupuesto <- presupuesto*(1+indice_precios_consumidor$inflacion[j+1])}
  actualizados <- c(actualizados, presupuesto)}
metodo2_nueva_inflacion['costo_presos23737_mensual_actualizado'] <- actualizados
sum(metodo2_nueva_inflacion$costo_presos23737_mensual_actualizado) # 28822343765

#METODO3
metodo3_nueva_inflacion <- metodo3
actualizados <- c()
for(i in 1:nrow(metodo3_nueva_inflacion)){
  presupuesto <- metodo3_nueva_inflacion$costo_presos23737_mensual_promedio[i]
  for(j in i:(nrow(indice_precios_consumidor)-1)){
    presupuesto <- presupuesto*(1+indice_precios_consumidor$inflacion[j+1])
  }
  actualizados <- c(actualizados, presupuesto)
}
metodo3_nueva_inflacion['costo_presos23737_mensual_promedio_actualizado'] <- actualizados 
sum(metodo3_nueva_inflacion$costo_presos23737_mensual_promedio_actualizado) # 28785599779

#COMPARATIVO
comparacion_nueva_inflacion <- ley23737_sobre_total$anio %>% 
  cbind(metodo1_nueva_inflacion %>% group_by(anio) %>% summarise(metodo1_actualizado= sum(costo_presos23737_mensual_actualizado)) %>% select(metodo1_actualizado)) %>%
  cbind(metodo2_nueva_inflacion %>% group_by(anio) %>% summarise(metodo2_actualizado= sum(costo_presos23737_mensual_actualizado)) %>% select(metodo2_actualizado)) %>%
  cbind(metodo3_nueva_inflacion %>% group_by(anio) %>% summarise(metodo3_actualizado= sum(costo_presos23737_mensual_promedio_actualizado)) %>% select(metodo3_actualizado))
colnames(comparacion_nueva_inflacion) <- c('anio','metodo1_31dic','metodo2_lineal','metodo3_promedio')
View(comparacion_nueva_inflacion)

openxlsx::write.xlsx(comparacion_nueva_inflacion, 'C:/Users/Usuario/Desktop/Lucas/1- pe/5-presos/DATA/comparacion_nueva_inflacion.xlsx')
