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



# RELACIONES SOCIALES E IGUALDAD ------------------------------------------


# CONFIANZA INTERPERSONAL -------------------------------------------------

# SE PUEDE CONFIAR EN LA MAYORIA DE LAS PERSONAS


attr(elsoc_bienestar$c02,"label")<- "Se puede confiar en las personas"
### EVOLUCION GENERAL
grafo_prop_ola("c02",c(1,33),1,"Grado de acuerdo por ola")

grafo_prop_var("c02","pp_3","Grado de acuerdo según voto en plebiscito", 
               umbral = 1,
               limy_sup = .5,
               atricion = c(1,33))

# ALTRUISMO SOCIAL GENERALIZADO
attr(elsoc_bienestar$c03,"label")<- "Las personas tratan de ayudar a los demás"

### EVOLUCION GENERAL 
grafo_prop_ola("c03",c(1,33),1,"Grado de acuerdo por ola")

grafo_prop_var("c03","pp_3","Grado de acuerdo según voto en plebiscito", 
               umbral = 1,
               atricion = c(1,33),
               limy_sup = .5)




# TRATO JUSTO -------------------------------------------------------------


grafo_prop_var("c35_01","pp_3",umbral = c(4,5),
               titulo="Grado de acuerdo según tipo de votante",
               atricion=c(33))


grafo_prop_var("c35_02","pp_3",umbral = c(4,5),
               titulo="Grado de acuerdo según tipo de votante",
               atricion=c(33))


grafo_prop_var("c35_03","pp_3",umbral = c(4,5),
               titulo="Grado de acuerdo según tipo de votante",
               atricion=c(33))

grafo_prop_var("c35_04","pp_3",umbral = c(4,5),
               titulo="Grado de acuerdo según tipo de votante",
               atricion=c(33))




trato_justo_clase <- c("d25_01","d25_02","d25_03")
names(trato_justo_clase)<- c("Pobres","Clase Media","Clase Alta")




lapply(1:length(trato_justo_clase), function(i){
  
  elsoc_bienestar%>%
    filter(tipo_atricion %in% c(33))%>%
    sjlabelled::as_label(ola) %>%
    stats(!!rlang::sym(trato_justo_clase[i]),by=c(ola,pp_4),stat="mean",na.rm=TRUE)%>%
    drop_na()%>%
    filter(pp_4 %in% c("Votante\nCrónico","No-Votante\nCrónico"))%>%
    mutate(variable=names(trato_justo_clase)[i])
})%>%
  bind_rows()%>%
  ggplot(aes(x=ola,y=stat,color=variable,group=variable,label=round(stat,2)))+
  geom_point()+
  geom_line()+
  geom_point()+
  geom_text_repel(nudge_y = .01, size = 3, color = 'black') +
  facet_wrap(~pp_4)+
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





trato_justo_edad_sexo <- c("d25_04","d25_05","d25_06")
names(trato_justo_edad_sexo)<- c("Jóvenes","Adultos Mayores","Mujeres")




lapply(1:length(trato_justo_edad_sexo), function(i){
  
  elsoc_bienestar%>%
    filter(tipo_atricion %in% c(33))%>%
    sjlabelled::as_label(ola) %>%
    stats(!!rlang::sym(trato_justo_edad_sexo[i]),by=c(ola,pp_4),stat="mean",na.rm=TRUE)%>%
    drop_na()%>%
    filter(pp_4 %in% c("Votante\nCrónico","No-Votante\nCrónico"))%>%
    mutate(variable=names(trato_justo_edad_sexo)[i])
})%>%
  bind_rows()%>%
  ggplot(aes(x=ola,y=stat,color=variable,group=variable,label=round(stat,2)))+
  geom_point()+
  geom_line()+
  geom_point()+
  geom_text_repel(nudge_y = .01, size = 3, color = 'black') +
  facet_wrap(~pp_4)+
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

