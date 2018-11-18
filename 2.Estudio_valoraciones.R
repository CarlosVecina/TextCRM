library(ggplot2)
library(ggrepel)
library(scales)
library(plotly)
library(tidyverse)
source("functions/ConseguirValoraciones.R")

bancos <- c("BBVA","Santander", "Bankinter", "Caixa", "EVO", "CajaMar")
reviews <- readRDS("reviewsBruto100.RDS")

valoraciones <- conseguirValoraciones(reviews)

bancoEstudiado <- "BBVA"
p <- plot_ly(valoraciones %>% filter(Banco==bancoEstudiado), labels = ~factor(Estrellas), values = ~Freq, type = 'pie', 
             marker = list(colors=c("#04B431", "#74DF00","#D7DF01", "#DF7401","#DF0101")),
             textposition = 'outside',textinfo = 'label+percent+value',sort=F) %>%
    layout(title = paste0('Valoraciones app ',bancoEstudiado),
           showlegend = T,
           autosize = F, width = 500, height = 500,
           # margin = list(l = 15, r = 20, b = 1, t = 10),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
# p

valoracionesMedia <- valoraciones %>% 
    mutate(Estrellas = as.numeric(Estrellas)) %>% 
    group_by(Banco) %>% 
    summarise(Media = sum(Freq*Estrellas)/sum(Freq),
              Desviacion = sqrt(sum((Estrellas - Media)**2 * Estrellas) / (sum(Estrellas)-1))) %>% 
    arrange(desc(Media))
valoracionesMedia

valoracionesPerc <- valoraciones %>% 
    group_by(Banco, Estrellas) %>% 
    summarise(Suma = sum(Freq)) %>% mutate(RealPerc = Suma/sum(Suma)*100)

ggplotly(
ggplot(valoracionesPerc, aes(x=Estrellas, y=RealPerc, fill=Banco)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    scale_fill_manual(values=c("#ff6600", "#043263", "#ffffff", "#56B4E9", "#000000","#ff3300" ))
)

###### Lolipop Por encima o por debajo de la media.

# Data Prep
mediaGlobal <- valoracionesMedia %>% select(Media) %>% summarise(Media = round(mean(Media),2)) %>% pull()
plus1_formatter <- function(x) {x +mediaGlobal}
valoracionesMedia <- valoracionesMedia %>% 
    mutate(EncimaDebajo = if_else(Media>=mediaGlobal,"Encima","Debajo")) %>% 
    # arrange(desc(Media))
    arrange(Media)
valoracionesMedia <- valoracionesMedia %>% mutate(DesvioMedia = round(Media - mediaGlobal,2),
                                                  Media = round(Media,2))
valoracionesMedia$Banco <- factor(valoracionesMedia$Banco, levels = valoracionesMedia$Banco)

# Diverging Barcharts
ggplot(valoracionesMedia, aes(x=Banco, y=DesvioMedia, label=Banco)) + 
    geom_bar(stat='identity', aes(fill=EncimaDebajo), width=.5)+
    scale_fill_manual(name="Valoracion", 
                      labels = c("Encima de la media", "Debajo de la media"), 
                      values = c("Encima"="#00ba38", "Debajo"="#f8766d")) + 
    labs(subtitle= paste0("La valoracion media es: ", round(mediaGlobal,2)), 
         title= "Valoracion de las diferentes aplicaciones 1-5") + 
    coord_flip()+ scale_y_continuous(labels=plus1_formatter)


ggplot(valoracionesMedia, aes(x=Banco, y=DesvioMedia, label=Media)) + 
    geom_point(stat='identity', aes(col=EncimaDebajo), size=14)  +
    geom_segment(aes(y = 0, 
                     x = Banco, 
                     yend = DesvioMedia, 
                     xend = Banco), 
                 color = "black") +
    scale_color_manual(name="Valoracion", 
                       labels = c("Por Encima de la Media", "Por Debajo de la Media"), 
                       values = c("Encima"="#00ba38", "Debajo"="#f8766d")) + 
    geom_text(color="black", size=5) +
    labs(title="Valoracion de APPs bancarias (1 a 5 estrellas)", 
         subtitle=paste0("Teniendo en cuenta las valoraciones con comentario. La valoracion media es: ", mediaGlobal),
         x="Bancos",
         y="Media") + 
    ylim(-2.5, 2.5) +
    coord_flip() + 
    scale_y_continuous(labels=plus1_formatter)
