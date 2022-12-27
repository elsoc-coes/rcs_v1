library(tidyverse)
library(sjmisc)
library(sjlabelled)
library(ggrepel)
library(ggalluvial)
library(survey)
library(elsoc)
library(lubridate)
library(viridis)
library(forcats)


elsoc_cohesion <- readRDS("inputs/cohesion social/base_cohesion.RDS")

source("inputs/general/funcion para graficar proporciones.R")
source("inputs/general/funcion para graficar olas.R")




gf_cs_bienestar_movilidad_evolucion <- elsoc_cohesion %>% 
  filter(tipo_atricion == 1) %>% 
  sjlabelled::as_label(ola) %>% 
  prop(x = mov_interg_rec, by = ola, na.rm = T) %>% 
  ggplot(aes(y = prop, x = ola, fill = fct_rev(mov_interg_rec), 
             label = as.character(scales::percent(prop, accuracy = .1)))) + 
  theme_bw() + 
  geom_col(position = 'Stack') +
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_fill_viridis_d(end = .8, direction = -1) +
  scale_y_continuous(labels = scales::percent) + 
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(position = position_stack(vjust = .5),
            size = 3, 
            color = rep(c('white', 'white', 'black'), 6)) + 
  theme(legend.position = 'top',
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0)) +
  labs(caption = 'Nota: La perspectiva de movilidad social corresponde a la diferencia entre el estatus social esperado del hijo\ny el estatus social subjetivo del encuestado')

saveRDS(gf_cs_bienestar_movilidad_evolucion,file="inputs/cohesion social/bienestar/gf_cs_bienestar_movilidad_evolucion.RDS")

gf_cs_bienestar_movilidad_perfiles <- elsoc_cohesion %>% 
  filter(tipo_atricion == 1) %>% 
  sjlabelled::as_label(ola) %>% 
  prop(x = mov_interg_rec, by = c(ola,pp_3), na.rm = T)%>%
  drop_na()%>%
  filter(mov_interg_rec%in%"Perspectiva de\nmovilidad ascendente")%>%
  ggplot(aes(y = prop, x = ola, color = pp_3, group=pp_3,
             label = as.character(scales::percent(prop, accuracy = .1))))+
  theme_bw() + 
  geom_text_repel(nudge_y = .01, size = 3, color = 'black') +
  geom_point(size = 1.75) + 
  geom_line() + 
  scale_y_continuous(labels = scales::percent,limits = c(.3,1)) +
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_color_viridis_d(end = .75, option = 'viridis') +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = 'top',
        legend.title = element_blank())

saveRDS(gf_cs_bienestar_movilidad_perfiles,file="inputs/cohesion social/bienestar/gf_cs_bienestar_movilidad_perfiles.RDS")


grafo_prop_var(elsoc_cohesion,"m43","pp_3",atricion = 1,
               umbral = c(4,5),
               "Sobrecarga por deudas, según tipo de votante",
               subtitulo = "Bastante o muy sobrecargado",
               limy_inf=0,limy_sup = .4,
               guardar = TRUE,
               "gf_cs_bienestar_deuda_perfiles")



grafo_prop_var(elsoc_cohesion,"s01","pp_3",atricion = 1,
               umbral = c(4,5),
               "Satisfacción vital, según tipo de votante",
               subtitulo = "Satisfecho o totalmente satisfecho",
               limy_inf=.25,limy_sup = 1,
               guardar = TRUE,
               "gf_cs_bienestar_satisfaccion_perfiles")



