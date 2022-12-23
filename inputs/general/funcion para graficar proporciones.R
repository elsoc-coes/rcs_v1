

grafo_prop_var <- function(base,var_x,var_y,umbral=c(4,5),titulo,subtitulo,atricion=1,limy_inf=0,
                           limy_sup=1,guardar=FALSE,guardar_como="grafico",imprimir=TRUE){
  grafico<-  base%>%
    filter(tipo_atricion %in% atricion)%>%
    sjlabelled::as_label(ola) %>%
    mutate(esfuerzo=as.numeric(!!rlang::sym(var_x) %in% umbral))%>%
    prop_list(esfuerzo,by=c(ola,!!rlang::sym(var_y)),na.rm=TRUE)%>%
    filter(value==1)%>%
    drop_na()%>%
    ggplot(aes(x=factor(ola),y=prop,color=!!rlang::sym(var_y),group=!!rlang::sym(var_y),
               label = scales::percent(prop, accuracy = .1)))+
    geom_point()+
    geom_line()+
    geom_text_repel(nudge_y = .01, size = 3, color = 'black') +
    theme_bw()+
    ylab(label = NULL) +
    xlab(label = NULL) +
    scale_color_viridis_d(begin = 0, end = .85, option = 'viridis')+
    scale_y_continuous(labels = scales::percent, limits = c(limy_inf,limy_sup))+
    theme(plot.caption = element_text(hjust = 0),
          legend.position = 'top',
          legend.title = element_blank())+
    ggtitle(titulo,
            subtitle = subtitulo)+
    labs(caption = 'Fuente: Elaboración propia en base a datos ELSOC 2016-2022.')
  
  
  if(guardar){
    
    saveRDS(grafico,
            file=paste0("inputs/cohesion social/bienestar/",
                        guardar_como,
                        ".RDS"))
  }
  
  if(imprimir){
    return(grafico)  
  }
  
  
}   
