library(tidyverse)
load('inputs/ELSOC_Long_2016_2022_v1.00_R.RData')
load('inputs/perfiles.RData')


volumen_red <- elsoc_long_2016_2022%>%
  select(idencuesta,ola,starts_with('r01'))%>%
  gather(key = "variable",value="valor",-idencuesta,-ola)%>%
  group_by(idencuesta,ola)%>%
  summarise(vol.red= sum(valor>1 & valor<8, na.rm=TRUE))%>%
  mutate(vol.red=car::recode(vol.red, "0=1; 1:2=2; 3:5=3; 6:10=4; 11:13=5"))

elsoc_cohesion <- elsoc_long_2016_2022 %>%
  mutate(pleb = case_when(c55 == 4 ~ 5,
                          c55 == 1 ~ 1,
                          c55 == 2 ~ 2,
                          c55 == 3 ~ 3,
                          c55 == -999 ~ 5,
                          c55 == -888 ~ 5,
                          c55 == -777 ~ 5,
                          c55 == -666 ~ 5,
                          c56 == -999 ~ 5,
                          c56 == -888 ~ 5,
                          c56 == -777 ~ 5,
                          c56 == -666 ~ 5,
                          c56 == 2 ~ 4,
                          c57 == 1 ~ 1,
                          c57 == 2 ~ 2,
                          c57 == 3 ~ 3,
                          c57 == -999 ~ 5,
                          c57 == -888 ~ 5,
                          c57 == -777 ~ 5,
                          c57 == -666 ~ 5))  %>%
  sjmisc::set_na(.,na=c(-666,-777,-888,-999))%>%
  group_by(idencuesta) %>%
  mutate(salida = ifelse(length(na.omit(pleb)) == 0, NA, max(pleb, na.rm = T))) %>%
  ungroup()%>% 
  purrr::map_at(.at = vars(starts_with('s11_0')), 
                .f = function(s) car::recode(s, "1 = 0; 2 = 1; 3 = 2; c(4, 5) = 3; c(-888, -999) = NA")) %>%
  as.data.frame() %>%  
  mutate(phq9 = (s11_01 + s11_02 + s11_03 + s11_04 + s11_05 + s11_06 + s11_07 + s11_08 + s11_09),
         depr = factor(car::recode(phq9, "0:4 = 1; 5:9 = 2; 10:14 = 3; 15:27 = 4"), 
                       levels = c(1,2,3,4),
                       labels = c('Sin sintomas o síntomas\nde depresión mínima', 
                                  'Síntomas de\ndepresion media', 
                                  'Síntomas de\ndepresion moderada', 
                                  'Síntomas de depresion\nmoderada-severa\no severa')),
         salida = factor(car::recode(salida, "1 = 1; 2 = 2; 3:5 = 3"), 
                         levels = 1:3,
                         labels = c("Apruebo", "Rechazo", "Otros")),
         sit_laboral = factor(car::recode(m02, "1:3 = 1; 6 = 2; c(4,5,7,8,9) = 3"),
                              levels = 1:3,
                              labels = c("Ocupados","Desocupados","Inactivos")),
         conf_inter = factor(car::recode((c02==1)+(c03==1)+(c04==2),
                                         "0=0;1=1;c(2,3)=2"),
                             labels = c("Baja","Media","Alta")),
         clase_sub=factor(case_when(c33 %in% c(1,2)~1,
                             c33==3~2,
                             c33 %in% c(4,5)~3),
                          labels=c("Baja","Media","Alta")),
         mov_interg = d01_03 - d01_01,
         mov_interg_rec = factor(car::recode(mov_interg, "-8:-1 = 1; 0 = 2; 1:10 = 3; else = NA"), 
                                 levels = c(1,2,3), 
                                 labels = c("Perpectiva de\nmovilidad descendente", "Perspectiva de\ninmovilidad social",
                                            "Perspectiva de\nmovilidad ascendente")))%>%
  left_join(perfiles, by = "idencuesta")%>%
  mutate(pp_4 = factor(pp_4, 
                       levels = 1:4,
                       labels = c("Votante\nCrónico", "Desafecto", "No-Votante\nCrónico", "Activado")),
         pp_3=factor(pp_3,levels = 1:3,labels = c("Votante Habitual","No-votante","Votante Reactivo")))%>%
  left_join(volumen_red, by=c("idencuesta","ola"))

saveRDS(elsoc_cohesion,file = "inputs/cohesion social/base_cohesion.RDS")