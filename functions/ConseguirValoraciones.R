conseguirValoraciones <- function(x){
valoracionBBVA <- table(x$BBVA$Valoracion) %>% 
    as.data.frame() %>% 
    arrange(desc(Var1)) %>% 
    mutate(Banco = "BBVA")

valoracionSantander <- table(x$Santander$Valoracion) %>% 
    as.data.frame() %>% 
    arrange(desc(Var1)) %>% 
    mutate(Banco = "Santander")

valoracionBankinter <- table(x$Bankinter$Valoracion) %>% 
    as.data.frame() %>% 
    arrange(desc(Var1)) %>% 
    mutate(Banco = "Bankinter")

valoracionCaixa <- table(x$Caixa$Valoracion) %>% 
    as.data.frame() %>% 
    arrange(desc(Var1)) %>% 
    mutate(Banco = "Caixa")

valoracionEVO <- table(x$EVO$Valoracion) %>% 
    as.data.frame() %>% 
    arrange(desc(Var1)) %>% 
    mutate(Banco = "EVO")

valoracionCajaMar <- table(x$CajaMar$Valoracion) %>% 
    as.data.frame() %>% 
    arrange(desc(Var1)) %>% 
    mutate(Banco = "CajaMar")


valoraciones <- bind_rows(valoracionBBVA, valoracionSantander, valoracionBankinter, valoracionCaixa,
                          valoracionEVO, valoracionCajaMar) %>% set_names(c("Estrellas", "Freq", "Banco"))

return(valoraciones)
}