
# Cargamos los paquetes y los datos
source("functions/LoadRequiredPackages.R")
LoadRequiredPackages(c("ggplot2","ggrepel","wordcloud","scales","plotly","reshape2","stringr","tidyverse","tidytext"))

todasReviewsTidy <- readRDS("data/todasReviewsTidy.RDS")
todasReviewsTidyFiltrado <- readRDS("data/todasReviewsTidyFiltrado.RDS")

# Cargamos los vocabularios con clasificacion de sentimiento
# Preformulados
gitHubDicc <- read.delim("data/SenDiccSp.txt",header = F) %>%
    set_names(c("Palabra","Fuerza","Sentimiento")) %>% mutate(Fuerza = if_else(Fuerza =="strongsubj",2,1),
                                                              Sentimiento = if_else(Sentimiento=="positive","Positivo","Negativo"))
# Customs
custPosDicc <- read.delim("data/Pos.txt",sep = ",") %>% 
    mutate(Sent = "Positivo") %>% set_names(c("Palabra","Fuerza","Sentimiento"))
cusNegDicc <- read.delim("data/Neg.txt",sep = ",") %>% 
    mutate(Sent = "Negativo") %>% set_names(c("Palabra","Fuerza","Sentimiento"))

customDicc <- bind_rows(custPosDicc,cusNegDicc) %>% replace_na(list(Fuerza = 1)) %>% 
    arrange(Palabra) %>% 
    group_by(Palabra) %>%
    slice(1)

# Los combinamos
allDict <- bind_rows(customDicc,gitHubDicc) %>% 
    arrange(Palabra) %>% 
    group_by(Palabra) %>%
    slice(1)

# Vemos los terminos segun que vocabulario escojamos
todasReviewsTidy %>% 
    inner_join(customDicc) %>% nrow()
todasReviewsTidy %>% 
    inner_join(allDict) %>% nrow()

sentimientoReviews <- todasReviewsTidy %>% 
    inner_join(allDict) %>% 
    count(Banco, Comentario = Comentario, Sentimiento,wt = Fuerza) %>% 
    spread(Sentimiento,n,fill=0) %>% 
    mutate(Sentimiento = Positivo - Negativo)

# Queremos ver el sentimiento medio de cada banco
sentimientoMedio <- sentimientoReviews %>% group_by(Banco) %>% summarise(SentimientoMedio = mean(Sentimiento)) %>% arrange(desc(SentimientoMedio))

sentimientoReviews$Banco <- factor(sentimientoReviews$Banco ,levels = sentimientoMedio$Banco, ordered = T)

ggplot(sentimientoReviews, aes(Comentario,Sentimiento,fill=Banco))+
    geom_col(show.legend = F) +
    facet_wrap(~Banco, ncol=2, scales ="free_x") + 
    scale_fill_manual(values=c("#043263", "#ff3300", "#56B4E9","#ff6600", "#000000", "#29a329")) + 
    geom_hline(yintercept = 0)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
    


contadorPalabrasSentimiento <- todasReviewsTidy %>% 
    inner_join(allDict) %>%  count(Palabra, Sentimiento, sort=T) %>% ungroup()

contadorPalabrasSentimiento <-  contadorPalabrasSentimiento %>% group_by(Sentimiento) %>% top_n(10) %>% 
    ungroup() %>%  mutate(Palabra = reorder(Palabra,n))
ggplot(contadorPalabrasSentimiento, aes(Palabra, n ,fill = Sentimiento)) +
    geom_col(show.legend = F) +
    facet_wrap(~Sentimiento, scales = "free_y") +
    coord_flip()

todasReviewsTidy %>% 
    inner_join(allDict) %>%  count(Palabra, Sentimiento, sort=T) %>% 
    acast(Palabra~Sentimiento, value.var="n",fill=0) %>% 
    comparison.cloud(colors = c("#e60000","#47d147"), max.words = 100)

