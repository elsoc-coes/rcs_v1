
grafo_prop_ola <- function(base,var_y,atricion,umbral, titulo,subtitulo,guardar=FALSE,
                           guardar_como='grafico',imprimir=TRUE,
                           limy_inf=0,
                           limy_sup=.75){
  
grafico <-base%>%
    filter(tipo_atricion %in% atricion)%>%
    sjlabelled::as_label(ola) %>%
    mutate(valor= !!rlang::sym(var_y) %in% umbral)%>%
    prop_list(valor,by=c(ola),na.rm=TRUE)%>%
    mutate(valor=as.numeric(value))%>%
    filter(value==1)%>%
    drop_na()%>%
    ggplot(aes(x=factor(ola),y=prop, group=value,
               label = scales::percent(prop, accuracy = .1)))+
    geom_point()+
    geom_line()+
    geom_text_repel(nudge_y = .01, size = 3) +
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
            file=paste0("inputs/cohesion social/relaciones-sociales/graficos/",
                        guardar_como,
                        ".RDS"))
  }
  
  if(imprimir){
    return(grafico)  
  }
  
  
  
}
  

