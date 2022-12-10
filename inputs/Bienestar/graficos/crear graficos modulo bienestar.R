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



