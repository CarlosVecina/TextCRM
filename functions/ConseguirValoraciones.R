
conseguirValoraciones <- function(x){
    
valoraciones <- x %>% 
    map2(.y=names(.),~.x %>% 
         group_by(Estrellas=Valoracion)  %>% 
         summarise(Freq=n()) %>% 
         mutate(Banco=.y)%>% 
         arrange(desc(Estrellas))) 
    %>% bind_rows()
    
return(valoraciones)
}
