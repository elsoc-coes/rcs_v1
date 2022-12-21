# TRATO JUSTO -------------------------------------------------------------


grafo_prop_var(elsoc_cohesion,"c35_01","pp_3",umbral = c(4,5),
               titulo="Grado de acuerdo según tipo de votante",
               atricion=1)


grafo_prop_var(elsoc_cohesion,"c35_02","pp_3",umbral = c(4,5),
               titulo="Grado de acuerdo según tipo de votante",
               atricion=1)


grafo_prop_var(elsoc_cohesion,"c35_03","pp_3",umbral = c(4,5),
               titulo="Grado de acuerdo según tipo de votante",
               atricion=1)

grafo_prop_var(elsoc_cohesion,"c35_04","pp_3",umbral = c(4,5),
               titulo="Grado de acuerdo según tipo de votante",
               atricion=1)




trato_justo_clase <- c("d25_01","d25_02","d25_03")
names(trato_justo_clase)<- c("Pobres","Clase Media","Clase Alta")




lapply(1:length(trato_justo_clase), function(i){
  
  elsoc_cohesion%>%
    filter(tipo_atricion %in% 1)%>%
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
  
  elsoc_cohesion%>%
    filter(tipo_atricion %in% 1)%>%
    sjlabelled::as_label(ola) %>%
    stats(!!rlang::sym(trato_justo_edad_sexo[i]),by=c(ola,pp_4),stat="mean",na.rm=TRUE)%>%
    drop_na()%>%
    filter(pp_4 %in% c("Votante\nCrónico","No-Votante\nCrónico"))%>%
    mutate(variable=names(trato_justo_edad_sexo)[i])})%>%
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

