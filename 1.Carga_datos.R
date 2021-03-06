
# 0. Levantamos la API
# 1. Descargamos los datos accediento a la API

library(dplyr)
library(tidyverse)
library(httr)
library(jsonlite)
bancos <- c("BBVA","Santander", "Bankinter", "Caixa", "EVO", "CajaMar")
bancosGooglePlay <- c("com.bbva.bbvacontigo","es.bancosantander.apps",
                  "com.bankinter.launcher", "es.lacaixa.mobile.android.newwapicon",
                   "es.evobanco.bancamovil", "com.grupocajamar.wefferent")


paginas <- 0:100


idioma <- "es"
reviewsBruto <- vector("list", length(bancos)) %>% set_names(bancos)

for(i in 1:length(bancos)){
    banco <- bancos[i]
    browser()
    for(pagina in paginas){
        
        url = paste0("http://localhost:3000/api/apps/",bancosGooglePlay[i], "/reviews/?page=", pagina,"&lang=", idioma)
        reviews <- fromJSON(txt = url) %>% as.data.frame() 
        reviewsParsed <- reviews %>% 
            select(NombreUsuario = results.userName, FechaReview = results.date, 
                   Valoracion = results.score,
                   Titulo = contains("results.title"), Mensaje = results.text, 
                   FechaRespuesta = contains("results.replyDate"), 
                   Respuesta = contains("results.replyText"))
        reviewsBruto[[banco]] <- reviewsBruto[[banco]] %>% 
            bind_rows(reviewsParsed) 
        if(pagina %% 10 == 0){
            Sys.sleep(5)
        }
    }
}

# Comprobamos que se han recogido los datos correctamente

reviewsLista %>% map_dfc(~.x %>% select(Valoracion) %>% summary)



