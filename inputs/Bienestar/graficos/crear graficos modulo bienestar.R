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
  filter(tipo_atricion==1,salida!="Otros")%>%
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
  filter(tipo_atricion==1)%>%
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
  ggtitle('Confianza interpersonal, según tipo de votante')+
  labs(caption = 'Fuente: Elaboración propia en base a datos ELSOC 2016-2022.')

saveRDS(gf_conf_inter_perfiles,file="inputs/Bienestar/graficos/gf_conf_inter_perfiles.RDS")



# MERITOCRACIA ------------------------------------------------------------


# PERSONAS SON RECOMEPENSADAS POR SU ESFUERZO
gf_meritocracia_esfuerzo_voto <- grafo_prop_var("c18_09","salida",
                                                titulo = 'Grado de acuerdo según voto',
                                                limy_sup = .60)


saveRDS(gf_meritocracia_esfuerzo_voto,
        file="inputs/Bienestar/graficos/gf_meritocracia_esfuerzo_voto.RDS")

# PERSONAS SON RECOMEPENSADAS POR SU ESFUERZO
gf_meritocracia_esfuerzo_perfiles <- grafo_prop_var("c18_09","pp_4",
                                    titulo = 'Grado de acuerdo según perfil votante',
                                     limy_sup =  .60)

saveRDS(gf_meritocracia_esfuerzo_perfiles,file="inputs/Bienestar/graficos/gf_meritocracia_esfuerzo_perfiles.RDS")




# PERSONAS RECOMPENSADAS POR SU INTELIGENCIA
gf_meritocracia_inteligencia_voto <-grafo_prop_var("c18_10","salida",
                                                   titulo = 'Grado de acuerdo según voto',
                                                   limy_sup =  .60)


saveRDS(gf_meritocracia_inteligencia_voto,
        file="inputs/Bienestar/graficos/gf_meritocracia_inteligencia_voto.RDS")

gf_meritocracia_inteligencia_perfiles <- grafo_prop_var("c18_10","pp_4",
                                                        titulo = 'Grado de acuerdo según perfil votante',
                                                        limy_sup =  .75)


saveRDS(gf_meritocracia_inteligencia_perfiles,
        file="inputs/Bienestar/graficos/gf_meritocracia_inteligencia_perfiles.RDS")



# PERCEPCION DE DESIGUALDAD -----------------------------------------------

grafo_prop_var("c18_11","salida",
               titulo = "Grado de acuerdo según voto",
               limy_low = .50,
               guardar = TRUE,
               guardar_como ="gf_desigualdad_ingreso_voto",
               imprimir = FALSE)


grafo_prop_var("c18_11","pp_4",
               titulo = "Grado de acuerdo según perfiles",
               limy_low = .50,
               guardar = TRUE,
               guardar_como ="gf_desigualdad_ingreso_perfiles",
               imprimir = FALSE)


# IDENTIDAD NACIONAL ------------------------------------------------------

grafo_prop_var("c32_01",
               "pp_4",
               titulo = "Grado de acuerdo según perfiles",
               limy_low = .5,
               guardar = TRUE,
               guardar_como = "gf_orgullo_chile_perfiles",
               imprimir = FALSE)


grafo_prop_var("c32_01",
               "salida",
               titulo = "Grado de acuerdo según voto",
               limy_low = .5,
               guardar = TRUE,
               guardar_como = "gf_orgullo_chile_voto",
               imprimir = FALSE)



# JUSTICIA DISTRIBUTIVA ---------------------------------------------------

grafo_prop_var("c32_01",
               "pp_4",
               titulo = "Grado de acuerdo según perfiles",
               limy_low = .5,
               guardar = TRUE,
               guardar_como = "gf_orgullo_chile_voto",
               imprimir = FALSE)


grafo_prop_var("d02_01",
               "salida",
               titulo = "Grado de acuerdo según voto",
               limy_sup = .5,
               guardar = TRUE,
               guardar_como = "gf_distributiva_pensiones_voto",
               imprimir = FALSE)

grafo_prop_var("d02_01",
               "pp_4",
               titulo = "Grado de acuerdo según perfiles",
               limy_sup = .5,
               guardar = TRUE,
               guardar_como = "gf_distributiva_pensiones_perfiles",
               imprimir = FALSE)



grafo_prop_var("d02_02",
               "salida",
               titulo = "Grado de acuerdo según voto",
               limy_sup = .4,
               guardar = TRUE,
               guardar_como = "gf_distributiva_educacion_voto",
               imprimir = FALSE)

grafo_prop_var("d02_02",
               "pp_4",
               titulo = "Grado de acuerdo según perfiles",
               limy_sup = .4,
               guardar = TRUE,
               guardar_como = "gf_distributiva_educacion_perfiles",
               imprimir = FALSE)


grafo_prop_var("d02_03",
               "salida",
               titulo = "Grado de acuerdo según voto",
               limy_sup = .5,
               guardar = TRUE,
               guardar_como = "gf_distributiva_salud_voto",
               imprimir = FALSE)

grafo_prop_var("d02_03",
               "pp_4",
               titulo = "Grado de acuerdo según perfiles",
               limy_sup = .5,
               guardar = TRUE,
               guardar_como = "gf_distributiva_salud_perfiles",
               imprimir = FALSE)


