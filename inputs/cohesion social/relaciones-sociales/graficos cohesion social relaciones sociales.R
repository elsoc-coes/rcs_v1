library(tidyverse)
library(sjmisc)
library(sjlabelled)
library(ggrepel)
library(ggalluvial)
library(survey)
library(elsoc)
library(lubridate)
library(viridis)


elsoc_cohesion <- readRDS("inputs/cohesion social/base_cohesion.RDS")

source("inputs/general/funcion para graficar proporciones.R")
source("inputs/general/funcion para graficar olas.R")



# RELACIONES SOCIALES E IGUALDAD ------------------------------------------


# CONFIANZA INTERPERSONAL -------------------------------------------------

# SE PUEDE CONFIAR EN LA MAYORIA DE LAS PERSONAS


attr(elsoc_cohesion$c02,"label")<- "Se puede confiar en las personas"
### EVOLUCION GENERAL
grafo_prop_ola(elsoc_cohesion,"c02",1,1,"Grado de acuerdo por ola",
               guardar = TRUE,
               guardar_como = "gf_cs_rsi_confianza_evolucion",
               imprimir = FALSE)

grafo_prop_var(elsoc_cohesion,"c02","pp_3","Grado de acuerdo según voto en plebiscito", 
               umbral = 1,
               limy_sup = .5,
               atricion = 1,
               guardar = TRUE,
               guardar_como = "gf_cs_rsi_confianza_perfiles",
               imprimir = FALSE)

# ALTRUISMO SOCIAL GENERALIZADO
attr(elsoc_cohesion$c03,"label")<- "Las personas tratan de ayudar a los demás"

### EVOLUCION GENERAL 
grafo_prop_ola(elsoc_cohesion,"c03",1,1,"Grado de acuerdo por ola",
               guardar = TRUE,
               guardar_como = "gf_cs_rsi_altruismo_evolucion",
               imprimir = FALSE)

grafo_prop_var(elsoc_cohesion,"c03","pp_3","Grado de acuerdo según voto en plebiscito", 
               umbral = 1,
               atricion =1,
               limy_sup = .5,
               guardar = TRUE,
               guardar_como = "gf_cs_rsi_altruismo_perfiles",
               imprimir = FALSE)


# RECONOCIMEINTO Y RESPETO DE LA DIVERSIDAD -------------------------------

# Grado de confianza en Homosexuales
### EVOLUCION GENERAL
grafo_prop_ola(elsoc_cohesion,"c06_04",1,c(4,5),"Grado de acuerdo por ola",
               guardar = TRUE,
               guardar_como = "gf_cs_rrd_homo_evolucion",
               imprimir = FALSE)

### Por perfil

grafo_prop_var(elsoc_cohesion,"c06_04","pp_3","Grado de acuerdo según voto en plebiscito", 
               umbral = c(4,5),
               atricion = 1,
               limy_sup = .5,
               guardar = TRUE,
               guardar_como = "gf_cs_rrd_homo_perfiles",
               imprimir = FALSE)


# Grado de confianza en Mapuche
### EVOLUCION GENERAL

grafo_prop_ola(elsoc_cohesion,"c06_05",c(1),c(4,5),"Grado de acuerdo por ola",
               guardar = TRUE,
               guardar_como = "gf_cs_rrd_mapu_evolucion",
               imprimir = FALSE)

grafo_prop_var(elsoc_cohesion,"c06_05","pp_3","Grado de acuerdo según voto en plebiscito", 
               umbral = c(4,5),
               atricion = 1,
               limy_sup = .5,
               guardar = TRUE,
               guardar_como = "gf_cs_rrd_mapu_perfiles",
               imprimir = FALSE)

# EVOLUCION SEGUN TIPO MIGRANTE


gf_cs_rrd_inmig_evolucion <- elsoc_cohesion %>%
  filter(!is.na(r16), tipo_atricion %in% 1, ola %in% 4:6) %>%  
  sjlabelled::as_label(ola, cuestion_mig) %>%
  prop(r16 %in% 4:5, by = c(ola, cuestion_mig), na.rm = T)  %>%
  ggplot(aes(y = prop, x = ola, color = cuestion_mig, group = cuestion_mig,
             label = scales::percent(prop, accuracy = .1))) +
  theme_bw() + 
  geom_text_repel(nudge_y = .01, size = 3, color = 'black') +
  geom_point(size = 1.75) + 
  geom_line() + 
  scale_y_continuous(labels = scales::percent, limits = c(0, .75)) +
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_color_viridis_d(end = .75, option = 'viridis') +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = 'top',
        legend.title = element_blank()) + 
  ggtitle('Confianza hacia inmigrantes, según nacionalidad de migrantes',
          subtitle = 'Porcentaje con Bastante o Mucha confianza en inmigrantes')


saveRDS(gf_cs_rrd_inmig_evolucion, 
        file="inputs/cohesion social/relaciones-sociales/graficos/gf_cs_rrd_inmig_evolucion.RDS")

gf_cs_rrd_inmig_perfiles <-  elsoc_cohesion %>%
  filter(!is.na(r16), tipo_atricion %in% 1, ola %in% 4:6) %>%  
  sjlabelled::as_label(ola, cuestion_mig) %>%
  prop(r16 %in% 4:5, by = c(pp_3,ola, cuestion_mig), na.rm = T)%>%
  filter(pp_3 %in% c("Votante\nHabitual","No-Votante"))%>%
  ggplot(aes(y = prop, x = ola, color = cuestion_mig, group = cuestion_mig,
             label = scales::percent(prop, accuracy = .1))) +
  theme_bw() + 
  geom_text_repel(nudge_y = .01, size = 3, color = 'black') +
  geom_point(size = 1.75) + 
  geom_line() + 
  scale_y_continuous(labels = scales::percent, limits = c(0, .75)) +
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_color_viridis_d(end = .75, option = 'viridis') +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = 'top',
        legend.title = element_blank())+
  facet_wrap(~pp_3)+
  ggtitle('Confianza hacia inmigrantes, según tipo de votante',
          subtitle = 'Porcentaje con Bastante o Mucha confianza en inmigrantes')


saveRDS(gf_cs_rrd_inmig_perfiles, 
        file="inputs/cohesion social/relaciones-sociales/graficos/gf_cs_rrd_inmig_perfiles.RDS")

