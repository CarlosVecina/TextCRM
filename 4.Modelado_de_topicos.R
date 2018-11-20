
source("functions/LoadRequiredPackages.R")
LoadRequiredPackages(c("ggplot2","ggrepel","scales","plotly","stringr","tidyverse","tidytext"))

bancos <- c("BBVA","Santander", "Bankinter", "Caixa", "EVO", "CajaMar")
reviewsLista <- readRDS("reviewsBruto100.RDS")

todasReviews <- reviewsLista %>% map2(names(.), ~ .x %>% mutate(Banco = .y)) %>% reduce(rbind)

# Eliminamos url y emails, y las tildes
eliminarUrl <- function(x) gsub("http[^[:space:]]*","",x)
eliminarEmail <- function(x) gsub("\\S+@\\S+","",x)

todasReviews <- todasReviews %>% mutate(Mensaje = eliminarEmail(eliminarUrl(Mensaje)))
todasReviews <- todasReviews %>% mutate(Mensaje= iconv(Mensaje,from="UTF-8",to="ASCII//TRANSLIT"))


### IDIOMA 

### STOP WORDS
stopwords::stopwords_getlanguages("stopwords-iso")
stopwords::stopwords_getsources()
stopwords_Snowball <- stopwords::stopwords(language = "es", source = "snowball") %>% as.tibble()
stopwords_Iso <- stopwords::stopwords(language = "es", source = "stopwords-iso") %>% as.tibble()
stopwords_Alternativo <- read.delim("countwirdsfree_dicc.txt") %>% pull(Palabra) %>% as.character() %>% as.tibble()

#https://jvera.rbind.io/post/2017/10/16/spanish-stopwords-for-tidytext-package/ OTRAS FUENTES
# stopwords_comunes <- Reduce(intersect, list(stopwords_Snowball,stopwords_Iso)) %>%  sort()
stopwords_comunes <- full_join(stopwords_Snowball,stopwords_Iso) %>% full_join(stopwords_Alternativo)
stopwords_comunes <- rbind(stopwords_comunes, "q", "mas", "solo","tener","parece","puede", "bastante","hace","NA",NA,
                       "gracias", "siempre", "dice", "bbva","evo","ok")
todasReviewsTidy <- todasReviews %>% 
    mutate(Comentario = row_number()) %>% 
    unnest_tokens(Palabra,Mensaje) %>% 
    anti_join(stopwords_comunes %>% as.tibble() %>% set_names("Palabra"))


#### Estudio de la longitud de los comentarios
todasReviewsTidy %>% count(Palabra, sort=T)
todasReviewsTidy %>% group_by(Comentario) %>% summarise(Longitud = n()) %>% summarise(Media = mean(Longitud))
todasReviewsTidy %>% group_by(Banco,Comentario) %>% summarise(Longitud = n()) %>% 
    summarise(Media = mean(Longitud)) %>% arrange(desc(Media))
todasReviewsTidy %>% group_by(Valoracion, Comentario) %>% summarise(Longitud = n()) %>% 
    summarise(Media = mean(Longitud)) %>% arrange(desc(Media))



#       filtramos las palabras tan repetidas que no nos dan informacion y aquellas poco repetidas con
#       cuidado de no introducir demasiada ambiguedad

############
### TF -TDF
todasReviewsTidy %>% 
    count(Banco, Palabra, sort= T)
todasReviewsTidy %>% 
    count(Banco, Palabra, sort= T) %>% 
    bind_tf_idf(Palabra, Banco, n) %>% 
    group_by(Banco) %>% 
    top_n(18) %>% 
    ungroup() %>% 
    mutate(Palabra = reorder(Palabra, idf)) %>% 
    ggplot(aes(Palabra,idf,fill=Banco)) +
    geom_col(show.legend = F) +
    facet_wrap(~Banco, scales = "free") +
    coord_flip()


library(stm)
library(quanteda)

dfm <- todasReviewsTidy %>% 
    count(Banco, Palabra, sort=T) %>% 
    cast_dfm(Banco, Palabra, n)

topic_mode <- stm::stm(dfm, K=3, init.type =  "LDA")
summary(topic_mode)

td_beta <- tidy(topic_mode)
td_beta %>% 
    group_by(topic) %>% 
    top_n(10) %>% 
    ungroup() %>% 
    mutate(term = reorder(term, beta)) %>% 
    ggplot(aes(term,beta,fill=topic)) +
    geom_col(show.legend = F) +
    facet_wrap(~topic, scales = "free") +
    coord_flip()



###### Limpiamos los datos
palabrasPocoUtilizadas <- todasReviewsTidy %>% count(Palabra, sort=T) %>% filter(n<3) %>% pull(Palabra)
palabrasDemasiadoUtilizadas <- todasReviewsTidy %>% count(Palabra, sort=T) %>% filter(n>1000) %>% pull(Palabra)
todasReviewsTidyFiltrado <- todasReviewsTidy %>% 
    filter(!(Palabra %in% palabrasPocoUtilizadas)) %>% 
    filter(!(Palabra %in% palabrasDemasiadoUtilizadas))
todasReviewsTidyFiltrado %>% count(Palabra, sort=T) 

############
### TF -TDF
todasReviewsTidyFiltrado %>% 
    count(Banco, Palabra, sort= T)
todasReviewsTidyFiltrado %>% 
    count(Banco, Palabra, sort= T) %>% 
    bind_tf_idf(Palabra, Banco, n) %>% 
    group_by(Banco) %>% 
    top_n(18) %>% 
    ungroup() %>% 
    mutate(Palabra = reorder(Palabra, idf)) %>% 
    ggplot(aes(Palabra,idf,fill=Banco)) +
    geom_col(show.legend = F) +
    facet_wrap(~Banco, scales = "free") +
    coord_flip()


library(stm)
library(quanteda)

dfm <- todasReviewsTidyFiltrado %>%
    count(Banco, Comentario,Palabra, sort=T) %>% 
    # filter(Banco == "EVO") %>% 
    cast_dfm(Comentario, Palabra, n)
    # cast_dfm(Banco, Palabra, n)

topic_mode <- stm::stm(dfm, K=3, init.type =  "LDA")
summary(topic_mode)

td_beta <- tidy(topic_mode)

td_beta %>% 
    group_by(topic) %>% 
    top_n(7) %>% 
    ungroup() %>% 
    mutate(term = reorder(term, beta)) %>% 
    ggplot(aes(term,beta,fill=topic)) +
    geom_col(show.legend = F) +
    facet_wrap(~topic, scales = "free") +
    coord_flip()

result <- ldatuning::FindTopicsNumber(
    dfm,
    topics = seq(from = 2, to = 15, by = 1),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = 2L,
    verbose = TRUE
)

ldatuning::FindTopicsNumber_plot(result)

# Documentos-Topico

ap_documents <- tidy(topic_mode, matrix = "gamma")
ap_documents

saveRDS(todasReviewsTidy,"data/todasReviewsTidy.RDS")
saveRDS(todasReviewsTidyFiltrado,"data/todasReviewsTidyFiltrado.RDS")
