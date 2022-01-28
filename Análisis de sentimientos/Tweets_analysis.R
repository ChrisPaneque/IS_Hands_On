library(dplyr)
library(stopwords)
library(tidytext)
library(stringi)
library(stringr)
library(ggplot2)
library(scales)
library(tidyr)
library(widyr)
library(ggraph)
library(igraph)
library(quanteda)
library(topicmodels)
library(cvTools)
library(tidyverse)
library(tm)
library(lubridate)
library(zoo)



# Leemos el csv afinn
afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1")
as_tibble(afinn)

#Importamos el dataset
library(readr)
tweets <- read_csv("C:/Users/Chris/Desktop/Master/Intelligent systems/Análisis de sentimientos/tweets.csv")

#limpiamos columnas nulas
tweets = tweets[-3] 
tweets = tweets[-6] 
tweets = tweets[-6] 

#Separamos la fecha para obtener los periodos
tweets <- separate(tweets, date_time, into = c("Fecha", "Hora"), sep = " ")
tweets <- separate(tweets, Fecha, into = c("Dia", "Mes", "Periodo"), sep = "/",
                    remove = FALSE)
#eliminamos valores nulos
tweets=na.omit(tweets)

#convertimos la fecha a tipo Date
tweets <- mutate(tweets, Fecha = dmy(Fecha))

#Separamos los tweets en palabras

tweets=unnest_tokens(tweets, input = "content", output = "Word", drop = FALSE) 

#Unimos los tweets con afinn usando la columna word

tweetsAfinn=inner_join(tweets, afinn, ., by = "Word") 

#Creamos la columna Puntuación según la palabra
tweetsAfinn=mutate(tweetsAfinn, Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa"))  


#Obtenemos también una puntuación por tweett, usando group_by() y summarise()

tweets <-
  tweetsAfinn %>%
  group_by(id) %>%
  summarise(P_tweet = mean(Puntuacion)) %>%
  left_join(tweets, ., by = "id") %>% 
  mutate(P_tweet = ifelse(is.na(P_tweet), 0, P_tweet))  

# Palabras en total y únicas por candidato y por periodo
tweetsAfinn %>%
  count(author)

tweetsAfinn %>% 
  group_by(author) %>% 
  distinct(Word) %>% 
  count()

tweetsAfinn %>% 
  group_by(Periodo) %>% 
  distinct(Word) %>% 
  count()

#Eliminamos las palabras "no" y "yes" para que no influyan en los análisis

tweetsAfinn <-
  tweetsAfinn %>%
  filter(Word != "no")


tweetsAfinn <-
  tweetsAfinn %>%
  filter(Word != "yes")


#Definimos un tema para facilitar la visualización de nuestros resultados.

tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))


# Generamos un wordcloud para ver las palabras mas usadas

library(wordcloud)
library(RColorBrewer)

wordcl <- tweetsAfinn %>% group_by(Word) %>% count(Word) %>%  
  group_by(Word) %>% mutate(frecuencia = n/dim(tweetsAfinn)[1])


# graficamos el wordcloud
wordcloud(words = wordcl$Word, freq = wordcl$frecuencia,
          max.words = 400, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))



#GRaficamos las palabras y su cantidad filtrando por candidato y si son "positivas" o "negativas"

map(c("Positiva", "Negativa"), function(sentimiento) {
  tweetsAfinn %>%
    filter(Tipo ==  sentimiento) %>%
    group_by(author) %>%
    count(Word, sort = T) %>%
    top_n(n = 10, wt = n) %>%
    ggplot() +
    aes(Word, n, fill = author) +
    geom_col() +
    facet_wrap("author", scales = "free") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = sentimiento) +
    tema_graf
})

#Filtramos los tweets dejando los del 2016 para analizar este año
tweets16=filter(tweetsAfinn, Periodo==2016)

# Obtenemos la media de sentimientos por día, para analizar tendencias

tweets_year <-
  tweets16 %>%
  group_by(id) %>%
  mutate(Suma = mean(Puntuacion)) %>%
  group_by(author, Fecha) %>%
  summarise(Media = mean(Puntuacion))

#Filtramos para analizar los tweets de cristiano, cnn e instagram(pueden ser otros, o más) 

tweets_years_filt=filter(tweets_year, author %in% c("Cristiano", "cnnbrk", "instagram"))

#Graficamos los resultados

tweets_years_filt %>%
  ggplot() +
  aes(Fecha, Media, color = author) +
  geom_line() +
  tema_graf +
  theme(legend.position = "top")

#separamos las gráficas para que sea mas comprensible

tweets_years_filt %>%
  ggplot() +
  aes(Fecha, Media, color = author) +
  geom_hline(yintercept = 0, alpha = .35) +
  geom_line() +
  facet_grid(author~.) +
  tema_graf +
  theme(legend.position = "none")

#hacemos regresión local usando las medias por dias para realizar suavizado y poder ver el comportamiento

tweets_years_filt %>%
  ggplot() +
  aes(Fecha, Media, color = author) +
  facet_grid(author~.) +
  geom_smooth(method = "loess", fill = NA) +
  tema_graf

#pumtos de regresión

tweets_years_filt %>%
  ggplot() +
  aes(Fecha, Media, color = author) +
  geom_point(color = "#E5E5E5") + 
  geom_smooth(method = "loess", fill = NA) +
  facet_wrap(~author) +
  tema_graf

