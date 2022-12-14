library(tidyverse)
library(sjmisc)
library(sjlabelled)
library(ggrepel)
library(ggalluvial)
library(survey)
library(elsoc)
library(lubridate)
library(viridis)


elsoc_bienestar<-  readRDS("inputs/Bienestar/base_bienestar.RDS")

source("inputs/Bienestar/graficos/funcion para graficar proporciones.R")


# GRAFICOS CONFIANZA INTERPERSONAL ----------------------------------------



gf_conf_inter_voto<-  elsoc_bienestar%>%
  filter(tipo_atricion %in% c(1,33),salida!="Otros")%>%
  sjlabelled::as_label(ola) %>%
  prop_list(conf_inter,by=c(ola,salida),na.rm=TRUE)%>%
  ggplot(aes(x=factor(ola),y=prop,color=value,group=value,
             label = scales::percent(prop, accuracy = .1)))+
  geom_point()+
  geom_line()+
  geom_text_repel(nudge_y = .01, size = 3, color = 'black') +
  facet_wrap(~salida)+
  theme_bw()+
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_color_viridis_d(begin = 0, end = .85, option = 'viridis')+
scale_y_continuous(labels = scales::percent, limits = c(0, .75))+
  theme(plot.caption = element_text(hjust = 0),
        legend.position = 'top',
        legend.title = element_blank())+
  ggtitle('Confianza interpersonal, según voto en plebiscito de salida')+
  labs(caption = 'Fuente: Elaboración propia en base a datos ELSOC 2016-2022.')

saveRDS(gf_conf_inter_voto,file="inputs/Bienestar/graficos/gf_conf_inter.RDS")

gf_conf_inter_perfiles <- elsoc_bienestar%>%
  filter(tipo_atricion %in% c(1,33))%>%
  sjlabelled::as_label(ola) %>%
  prop_list(conf_inter,by=c(ola,pp_4),na.rm=TRUE)%>%
  filter(value=="Baja")%>%
  drop_na()%>%
  ggplot(aes(x=factor(ola),y=prop,color=pp_4,group=pp_4,
             label = scales::percent(prop, accuracy = .1)))+
  geom_point()+
  geom_line()+
  geom_text_repel(nudge_y = .01, size = 3, color = 'black') +
  theme_bw()+
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_color_viridis_d(begin = 0, end = .85, option = 'viridis')+
  scale_y_continuous(labels = scales::percent, limits = c(.25, 1))+
  theme(plot.caption = element_text(hjust = 0),
        legend.position = 'top',
        legend.title = element_blank())+
  ggtitle('Confianza interpersonal, según tipo de votante',
          subtitle="Porcentaje de votantes con baja confianza interpersonal")+
  labs(caption = 'Fuente: Elaboración propia en base a datos ELSOC 2016-2022.')

saveRDS(gf_conf_inter_perfiles,file="inputs/Bienestar/graficos/gf_conf_inter_perfiles.RDS")

# ALTRUISMO SOCIAL GENERALIZADO

attr(elsoc_bienestar$c03,"label")<- "Las personas tratan de ayudar a los demás"
grafo_prop_var("c03","salida","Grado de acuerdo según voto en plebiscito", 
               umbral = 1,
               atricion = c(1,33),
               limy_sup = .5,
               guardar = TRUE,
               guardar_como = "gf_altruismo_voto",
               imprimir = FALSE)


grafo_prop_var("c03","pp_4","Grado de acuerdo según tipo de votante", 
               umbral = 1,
               atricion = c(1,33),
               limy_sup = .5,
               guardar = TRUE,
               guardar_como = "gf_altruismo_perfiles",
               imprimir = FALSE)

# JUSTICIA SOCIAL
attr(elsoc_bienestar$c04,"label")<- "Las personas tratan de ser justas"

grafo_prop_var("c04","salida","Grado de acuerdo según voto en plebiscito", 
               umbral = 1,
               atricion = c(1,33),
               guardar = TRUE,
               guardar_como = "gf_justicia_social_voto",
               imprimir = FALSE)


grafo_prop_var("c04","pp_4","Grado de acuerdo según tipo de votante", 
               umbral = 1,
               atricion = c(1,33),
               guardar = TRUE,
               guardar_como = "gf_justicia_social_perfiles",
               imprimir = FALSE)

# CONFIANZA SOCIAL GENERALIZADA

attr(elsoc_bienestar$c02,"label")<- "Se puede confiar en las personas"

grafo_prop_var("c02","salida","Grado de acuerdo según voto en plebiscito", 
               umbral = 1,
               limy_sup = .5,
               atricion = c(1,33),
               guardar = TRUE,
               guardar_como = "gf_confianza_social_voto",
               imprimir = FALSE)

grafo_prop_var("c02","pp_4","Grado de acuerdo según tipo de votante", 
               umbral = 1,
               limy_sup = .5,
               atricion = c(1,33),
               guardar = TRUE,
               guardar_como = "gf_confianza_social_perfiles",
               imprimir = FALSE)





# MERITOCRACIA ------------------------------------------------------------


# PERSONAS SON RECOMEPENSADAS POR SU ESFUERZO
grafo_prop_var("c18_09","salida",
               titulo = 'Grado de acuerdo según voto en plebiscito',

              limy_sup = .60,
              atricion = c(1,33),
              guardar = TRUE,
              guardar_como="gf_meritocracia_esfuerzo_voto",
              imprimir=FALSE)


 grafo_prop_var("c18_09","pp_4",
    titulo = 'Grado de acuerdo según tipo de votante',
    limy_sup =  .60,
    atricion = c(1,33),
    guardar = TRUE,
    guardar_como="gf_meritocracia_esfuerzo_perfiles",
    imprimir=FALSE)


# PERSONAS RECOMPENSADAS POR SU INTELIGENCIA
grafo_prop_var("c18_10","salida",
               titulo = 'Grado de acuerdo según voto en plebiscito',
               limy_sup =  .60,
               atricion = c(1,33),
               guardar = TRUE,
               guardar_como="gf_meritocracia_inteligencia_voto",
               imprimir=FALSE)



grafo_prop_var("c18_10","pp_4",
               titulo = 'Grado de acuerdo según tipo de votante',
               limy_sup =  .75,
               atricion = c(1,33),
               guardar = TRUE,
               guardar_como="gf_meritocracia_inteligencia_perfiles",
               imprimir=FALSE)


# PERCEPCION DE DESIGUALDAD -----------------------------------------------

grafo_prop_var("c18_11","salida",
               titulo = "Grado de acuerdo según voto en plebiscito",
               limy_low = .50,
               atricion = c(1,33),
               guardar = TRUE,
               guardar_como ="gf_desigualdad_ingreso_voto",
               imprimir = FALSE)


grafo_prop_var("c18_11","pp_4",
               titulo = "Grado de acuerdo según tipo de votante",
               limy_low = .50,
               atricion=c(1,33),
               guardar = TRUE,
               guardar_como ="gf_desigualdad_ingreso_perfiles",
               imprimir = FALSE)


# IDENTIDAD NACIONAL ------------------------------------------------------

grafo_prop_var("c32_01",
               "pp_4",
               titulo = "Grado de acuerdo según tipo de votante",
               limy_low = .5,
               atricion = c(1,33),
               guardar = TRUE,
               guardar_como = "gf_orgullo_chile_perfiles",
               imprimir = FALSE)


grafo_prop_var("c32_01",
               "salida",
               titulo = "Grado de acuerdo según voto en plebiscito",
               limy_low = .5,
               atricion=c(1,33),
               guardar = TRUE,
               guardar_como = "gf_orgullo_chile_voto",
               imprimir = FALSE)






grafo_prop_var("c32_01",
               "pp_4",
               titulo = "Grado de acuerdo según tipo de votante",
               limy_low = .5,
               guardar = TRUE,
               atricion = c(1,33),
               guardar_como = "gf_orgullo_chile_voto",
               imprimir = FALSE)

# JUSTICIA DISTRIBUTIVA ---------------------------------------------------
attr(elsoc_bienestar$d02_01,"label")<- "Es justo que las personas de altos ingresos tengan mejores pensiones" 

grafo_prop_var("d02_01",
               "salida",
               titulo = "Grado de acuerdo según voto en plebsicito",
               limy_sup = .5,
               guardar = TRUE,
               atricion=c(1,33),
               guardar_como = "gf_distributiva_pensiones_voto",
               imprimir = FALSE)

grafo_prop_var("d02_01",
               "pp_4",
               titulo = "Grado de acuerdo según tipo de votante",
               limy_sup = .5,
               guardar = TRUE,
               atricion = c(1,33),
               guardar_como = "gf_distributiva_pensiones_perfiles",
               imprimir = FALSE)


attr(elsoc_bienestar$d02_02,"label")<- "Es justo que las personas de altos ingresos tengan mejor educación" 


grafo_prop_var("d02_02",
               "salida",
               titulo = "Grado de acuerdo según voto en plebiscito",
               limy_sup = .4,
               guardar = TRUE,
               atricion=c(1,33),
               guardar_como = "gf_distributiva_educacion_voto",
               imprimir = FALSE)

grafo_prop_var("d02_02",
               "pp_4",
               titulo = "Grado de acuerdo según tipo de votante",
               limy_sup = .4,
               guardar = TRUE,
               atricion = c(1,33),
               guardar_como = "gf_distributiva_educacion_perfiles",
               imprimir = FALSE)


attr(elsoc_bienestar$d02_03,"label")<- "Es justo que las personas de altos ingresos tengan mejor salud" 


grafo_prop_var("d02_03",
               "salida",
               titulo = "Grado de acuerdo según voto en plebiscito",
               limy_sup = .5,
               guardar = TRUE,
               atricion = c(1,33),
               guardar_como = "gf_distributiva_salud_voto",
               imprimir = FALSE)

grafo_prop_var("d02_03",
               "pp_4",
               titulo = "Grado de acuerdo según tipo de votante",
               limy_sup = .5,
               guardar = TRUE,
               atricion = c(1,33),
               guardar_como = "gf_distributiva_salud_perfiles",
               imprimir = FALSE)

# Clase social subjetiva --------------------------------------------------


gf_clase_subjetiva_salida <- elsoc_bienestar%>%
  filter(tipo_atricion %in% c(33))%>%
  sjlabelled::as_label(ola) %>%
  prop_list(clase_sub,by=c(ola,salida),na.rm=TRUE)%>%
  drop_na()%>%
  filter(salida != "Otros")%>%
  ggplot(aes(x=ola,y=prop,color=value,group=value,
             label = scales::percent(prop, accuracy = .1)))+
  geom_line()+
  geom_point()+
  geom_text_repel(nudge_y = .01, size = 3, color = 'black') +
  facet_wrap(~salida)+
  theme_bw()+
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_color_viridis_d(begin = 0, end = .85, option = 'viridis')+
  scale_y_continuous(labels = scales::percent)+
  theme(plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        legend.position = "top")+
  ggtitle('Clase social con la que se identifica el entrevistado,\nsegún voto en plebiscito de salida')+

  labs(caption = 'Fuente: Elaboración propia en base a datos ELSOC 2016-2022.\nDatos correspondientes a la muestra de Refresco')


saveRDS(gf_clase_subjetiva_salida,file="inputs/Bienestar/graficos/gf_clase_subjetiva_salida.RDS")

#IDENTIFICACION CON CLASE SUBJETIVA

grafo_prop_var("c34","salida",umbral = c(4,5),atricion = c(33),
               titulo="Grado de acuerdo según votante",
               guardar = TRUE,
               guardar_como = "gf_identificacion_clase_subjetiva_salida",
               imprimir = FALSE)



# MOVILIDAD SOCIAL --------------------------------------------------------

vars_clase_sub <- c("d01_01","d01_02","d01_03")
names(vars_clase_sub)<- c("Entrevistado","Familia Origen","Hijos")




gf_ubicacion_social_salida <- lapply(1:length(vars_clase_sub), function(i){
  
  elsoc_bienestar%>%
    filter(tipo_atricion %in% c(1,33))%>%
    sjlabelled::as_label(ola) %>%
    stats(!!rlang::sym(vars_clase_sub[i]),by=c(ola,salida),stat="mean",na.rm=TRUE)%>%
    drop_na()%>%
    filter(salida!="Otros")%>%
    mutate(variable=names(vars_clase_sub)[i])
})%>%
  bind_rows()%>%
ggplot(aes(x=ola,y=stat,color=variable,group=variable,label=round(stat,2)))+
  geom_point()+
  geom_line()+
  geom_point()+
  geom_text_repel(nudge_y = .01, size = 3, color = 'black') +
  facet_wrap(~salida)+
  theme_bw()+
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_color_viridis_d(begin = 0, end = .85, option = 'viridis')+
  theme(plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        legend.position = "top")+
  ggtitle('Ubicación en la sociedad chilena según voto en plebiscito de salida',
          subtitle = "¿Dónde se ubicaría usted, sus hijos, y su familia de origen\n en la sociedad chilena?")+
  labs(caption = 'Fuente: Elaboración propia en base a datos ELSOC 2016-2022.\nPromedio en escala 0 a 10')

saveRDS(gf_ubicacion_social_salida, file="inputs/Bienestar/graficos/gf_ubicacion_social_salida.RDS")

gf_ubicacion_social_perfiles <-  lapply(1:length(vars_clase_sub), function(i){
  
  elsoc_bienestar%>%
    filter(tipo_atricion %in% c(1,33))%>%
    sjlabelled::as_label(ola) %>%
    stats(!!rlang::sym(vars_clase_sub[i]),by=c(ola,pp_4),stat="mean",na.rm=TRUE)%>%
    drop_na()%>%
    filter(pp_4 %in% c("Votante\nCrónico","No-Votante\nCrónico" ))%>%
    mutate(variable=names(vars_clase_sub)[i])
})%>%
  bind_rows()%>%
  ggplot(aes(x=ola,y=stat,color=variable,group=variable,label=round(stat,2)))+
  geom_point()+
  geom_line()+
  geom_point()+
  facet_wrap(~pp_4)+
  geom_text_repel(nudge_y = .01, size = 3, color = 'black') +
  theme_bw()+
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_color_viridis_d(begin = 0, end = .85, option = 'viridis')+
  theme(plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        legend.position = "top")+
  ggtitle('Ubicación en la sociedad chilena según perfil de voto',
  subtitle = "¿Dónde se ubicaría usted, sus hijos, y su familia de origen\n en la sociedad chilena?")+
  labs(caption = 'Fuente: Elaboración propia en base a datos ELSOC 2016-2022.\nPromedio en escala 0 a 10')


saveRDS(gf_ubicacion_social_perfiles,
        file="inputs/Bienestar/graficos/gf_ubicacion_social_perfiles.RDS")
# SEGURIDAD ECONOMMICA ----------------------------------------------------

attr(elsoc_bienestar$m44,"label")<- "Mi hogar NO tendría ahorros suficientes para arreglárselas durante los próximos tres meses \n sin que algún miembro del hogar trabaje"

grafo_prop_var("m44","salida",umbral = c(1),
               titulo="Grado de acuerdo según votante",
               atricion = c(1,33),
               limy_sup = .75,
               guardar = TRUE,
               guardar_como = "gf_ahorros_salida",
               imprimir = FALSE)

grafo_prop_var("m44","pp_4",umbral = c(1),
               titulo="Grado de acuerdo según perfil",
               limy_sup = .75,
               atricion = c(1,33),
               guardar = TRUE,
               guardar_como = "gf_ahorros_perfiles",
               imprimir=FALSE)

attr(elsoc_bienestar$m43,"label") <-"Porcentaje que se siente algo, bastante o muy sobrecargado por deudas"

grafo_prop_var("m43","salida",umbral = c(3,4,5),
               titulo="Sobrecarga financiera según voto en plebiscito",

               limy_sup = .75,
               atricion = c(1,33),
               guardar = TRUE,
               guardar_como = "gf_sobrecarga_deuda_salida",
               imprimir = FALSE)




grafo_prop_var("m43","pp_4",umbral = c(3,4,5),
               titulo="Grado de acuerdo según tipo de votante",
               limy_sup = .60,
               atricion=c(1,33),
               guardar = TRUE,
               guardar_como = "gf_sobrecarga_deuda_perfiles",
               imprimir = FALSE)



# PERCEPCION SALARIOS -----------------------------------------------------



salario_percibido <- c("d03_01","d03_02")
names(salario_percibido)<- c("Empresario","Obrero")

gf_salario_percibido_perfiles <- lapply(1:length(salario_percibido),
                                        function(i){
                                          elsoc_bienestar%>%
                                            filter(tipo_atricion %in% c(1,33),d03_01!=0)%>%
                                            mutate(d03_01 = ifelse(d03_01==0,1,d03_01),
                                                   d03_02 = ifelse(d03_02==0,1,d03_02)) %>%
                                            sjlabelled::as_label(ola) %>%
                                            mutate(log_salario=log(!!rlang::sym(salario_percibido[i])))%>%
                                            stats(log_salario,by=c(ola,pp_4),stat="mean",na.rm=TRUE)%>%
                                            drop_na() %>%
                                            mutate(variable=names(salario_percibido[i]))})%>%
  bind_rows()%>%
  filter(pp_4 %in% c("Votante\nCrónico","No-Votante\nCrónico"))%>%
  ggplot(aes(x=ola,y=stat,color=variable,group=variable,label=round(stat,2)))+
  geom_line()+
  geom_point()+
  geom_text_repel(nudge_y = .01, size = 3, color = 'black') +
  theme_bw()+
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_color_viridis_d(begin = 0, end = .85, option = 'viridis')+
  facet_wrap(~pp_4)+
  theme(plot.caption = element_text(hjust = 0),
        legend.position = 'top',
        legend.title = element_blank())+
  ggtitle("Percepcion salario (Logaritmo) según tipo de votante",
          subtitle = "¿Cuánto cree que gana un Empresario y un Obrero?")+
  labs(caption = 'Fuente: Elaboración propia en base a datos ELSOC 2016-2022.')

saveRDS(gf_salario_percibido_perfiles,
        file="inputs/Bienestar/graficos/gf_salario_percibido_perfiles.RDS")



gf_salario_percibido_salida <-  lapply(1:length(salario_percibido),function(i){
  elsoc_bienestar%>%
    mutate(d03_01 = ifelse(d03_01==0,1,d03_01),
           d03_02 = ifelse(d03_02==0,1,d03_02)) %>%
    filter(tipo_atricion %in% c(1,33),d03_01!=0)%>%
    sjlabelled::as_label(ola) %>%
    mutate(log_salario=log(!!rlang::sym(salario_percibido[i])))%>%
    stats(log_salario,by=c(ola,salida),stat="mean",na.rm=TRUE)%>%
    drop_na()%>%
    mutate(variable=names(salario_percibido[i]))})%>%
  bind_rows()%>%
  filter(salida %in% c("Apruebo","Rechazo"))%>%
  ggplot(aes(x=ola,y=stat,color=variable,group=variable,label=round(stat,2)))+
  geom_line()+
  geom_point()+
  geom_text_repel(nudge_y = .01, size = 3, color = 'black') +
  theme_bw()+
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_color_viridis_d(begin = 0, end = .85, option = 'viridis')+
  facet_wrap(~salida)+
  theme(plot.caption = element_text(hjust = 0),
        legend.position = 'top',
        legend.title = element_blank())+
  ggtitle("Percepcion salario (Logaritmo) según voto en plebiscito",
  subtitle = "¿Cuánto cree que gana un Empresario y un Obrero?")+
  labs(caption = 'Fuente: Elaboración propia en base a datos ELSOC 2016-2022.')



saveRDS(gf_salario_percibido_salida,
        file="inputs/Bienestar/graficos/gf_salario_percibido_salida.RDS")


saveRDS(gf_salario_percibido_salida,
        file="inputs/Bienestar/graficos_gf_salario_percibido_salida.RDS")

salario_justo <- c("d04_01","d04_02")
names(salario_justo)<- c("Empresario","Obrero")


gf_salario_justo_perfiles <- lapply(1:length(salario_justo),function(i){
  elsoc_bienestar%>%
    filter(tipo_atricion %in% c(1,33))%>%
    sjlabelled::as_label(ola) %>%
    mutate(log_salario=log(!!rlang::sym(salario_justo[i])))%>%
    stats(log_salario,by=c(ola,pp_4),stat="mean",na.rm=TRUE)%>%
    drop_na()%>%
    mutate(variable=names(salario_justo[i]))})%>%
  bind_rows()%>%
  filter(pp_4 %in% c("Votante\nCrónico","No-Votante\nCrónico"))%>%
  ggplot(aes(x=ola,y=stat,color=variable,group=variable,label=round(stat,2)))+
  geom_line()+
  geom_point()+
  geom_text_repel(nudge_y = .01, size = 3, color = 'black') +
  theme_bw()+
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_color_viridis_d(begin = 0, end = .85, option = 'viridis')+
  facet_wrap(~pp_4)+
  theme(plot.caption = element_text(hjust = 0),
        legend.position = 'top',
        legend.title = element_blank())+
  ggtitle("Salario justo (Logaritmo) según tipo de votante",
          subtitle = "¿Cuánto cree que debiese ganar un Empresario y un Obrero?")+
  labs(caption = 'Fuente: Elaboración propia en base a datos ELSOC 2016-2022.')

saveRDS(gf_salario_justo_perfiles,file = "inputs/Bienestar/graficos/gf_salario_justo_perfiles.RDS")





    gf_salario_justo_salida <- lapply(1:length(salario_justo),function(i){
  elsoc_bienestar%>%
    filter(tipo_atricion %in% c(1,33))%>%
    sjlabelled::as_label(ola) %>%
    mutate(log_salario=log(!!rlang::sym(salario_justo[i])))%>%
    stats(log_salario,by=c(ola,salida),stat="mean",na.rm=TRUE)%>%
    drop_na()%>%
    mutate(variable=names(salario_justo[i]))})%>%
  bind_rows()%>%
  filter(salida %in% c("Apruebo","Rechazo"))%>%
  ggplot(aes(x=ola,y=stat,color=variable,group=variable,label=round(stat,2)))+
  geom_line()+
  geom_point()+
  geom_text_repel(nudge_y = .01, size = 3, color = 'black') +
  theme_bw()+
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_color_viridis_d(begin = 0, end = .85, option = 'viridis')+
  facet_wrap(~salida)+
  theme(plot.caption = element_text(hjust = 0),
        legend.position = 'top',
        legend.title = element_blank())+
  ggtitle("Salario justo (Logaritmo) según voto en Plebiscito",
          subtitle = "¿Cuanto cree que debiese ganar un Obrero  y un Empresario?")+
  labs(caption = 'Fuente: Elaboración propia en base a datos ELSOC 2016-2022.')

saveRDS(gf_salario_justo_salida,file="inputs/Bienestar/graficos/gf_salario_justo_salida.RDS")

