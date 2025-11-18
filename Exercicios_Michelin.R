require(dplyr)
require(readr)
require(tidyverse)
library(jsonlite)
require(magrittr)
require(pracma)

dados <- read.csv("michelin.csv", header = T, sep = ",")
View(dados)

install.packages("pracma")

dados <- dados %>%
  mutate(lat_rad = deg2rad(Latitude)) %>%
  mutate(Long_rad = deg2rad(Longitude))

haversine_distance <- function(lat_radian, Long_radian) {
  d = 2*6371*asin(sqrt(sin((lat_radian - 0.8065012)/2)^2 + cos(0.8065012)*cos(lat_radian)*sin((Long_radian - 0.1073363)/2)^2))
  return(d)
}

dados %>%
  filter(Name == "Picchi")

dados <- dados %>%
  mutate(distancia = haversine_distance(lat_radian = lat_rad, Long_radian = Long_rad))

#Distância minima culinária creative (Corrigido)
dados %>%
  group_by(Cuisine) %>%
  filter(Location != "Vienna, Austria") %>%
  filter(str_detect(Cuisine, "Creative")) %>%
  arrange(distancia) %>%
  select(Name, Cuisine, distancia)

#Quantos restaurantes 1,2 ou 3 michelin até 100km (Corrigido) 
dados %>%
  group_by(Award) %>%
  filter(Award %in% c("3 Stars,Green Star", "3 Stars" ,"2 Stars", "2 Stars,Green Star", "1 Star", "1 Star,Green Star")) %>%
  filter(distancia <= 100) %>%
  select(Name, distancia)

#Restaurantes fora da cidade inicial, até 2 moedas, até 2000km, com pelo menos 1 estrela (Corrigido)
dados %>% 
  group_by(Price) %>%
  filter(Location != "Venice, Italy" & distancia <= 2000) %>%
  filter(Price %in% c("$$", "££", "¥¥", "₩₩", "€€","฿฿", "₫₫", "₺₺"))%>%
  filter(Award %in% c("3 Stars,Green Star", "3 Stars" ,"2 Stars", "2 Stars,Green Star", "1 Star", "1 Star,Green Star")) %>%
  select(Name, distancia, Award)

unique(dados$Award)

dados %>%
  group_by(Award)%>%
  filter(Award %in% c("1 Star", "1 Star,Green Star")) %>%
  filter(Location != "Vienna, Austria") %>%
  arrange(distancia) %>%
  select(Name, distancia, Award)


### Restaurante Venice, Italy
#Distancia para o restaurente mais proximo de 2 stars

dados %>%
  group_by(Award)%>%
  filter(Award %in% c("2 Stars", "2 Stars,Green Star")) %>%
  filter(Location != "Venice, Italy") %>%
  arrange(distancia) %>%
  select(Name, distancia, Award)

unique(dados$Award)

#Existe no total quantos restaurantes de 1,2 e 3 estrelas menos de 1000km
dados %>%
  group_by(Award)%>%
  filter(Award %in% c("3 Stars,Green Star", "3 Stars" ,"2 Stars", "2 Stars,Green Star", "1 Star", "1 Star,Green Star")) %>%
  filter(distancia <= 1000) %>%
  select(Name, distancia, Award)

#Quantos restaurantes, fora da cidade local, até 3 dinheiros, pelo menos uma estrela, distancia maxima de 3000
dados %>%
  group_by(Price) %>%
  filter(Price %ni% c("$$$$", "££££", "¥¥¥¥", "₩₩₩₩", "€€€€", "฿฿฿฿", "₫₫₫₫", "₺₺₺"))%>%
  filter(Location != "Rome, Italy") %>%
  filter(distancia <= 3000) %>%
  filter(Award %in% c("3 Stars,Green Star", "3 Stars" ,"2 Stars", "2 Stars,Green Star", "1 Star", "1 Star,Green Star")) %>%
  select(Name, distancia, Award)

#Culinaria contemporary distancia minima
dados %>%
  group_by(Cuisine) %>%
  filter(Location != "Venice, Italy") %>%
  filter(str_detect(Cuisine, "Contemporary")) %>%
  arrange(distancia) %>%
  select(Name, distancia, Cuisine)


#Dijon, France

dados$Award

dados <- dados %>%
  mutate(distancia = haversine_distance(lat_radian = lat_rad, Long_radian = Long_rad))

#Km para o próximo restaurante de 1 Star
dados %>%
  group_by(Award) %>%
  filter(Award %in% c("1 Star", "1 Star,Green Star")) %>%
  filter(Location != "Dijon, France") %>%
  arrange(distancia) %>%
  select(Name, Award, distancia)

#Quantos restaurantes a 3000km do restaurante inicial com 1,2 ou 3 estrelas
dados %>%
  group_by(Award) %>%
  filter(Award %in% c("3 Stars,Green Star", "3 Stars", "2 Stars", "2 Stars,Green Star", "1 Star", "1 Star,Green Star")) %>%
  filter(distancia <= 3000) %>%
  select(Name, Award, distancia)

#Restaurantes com pelo menos 1 Star, até dois dinheiros, distancia máxima 2000, fora da cidade inicial
dados%>%
  group_by(Price) %>%
  filter(Price %ni% c("₺","₺₺", "$","$$","£", "££" , "¥","¥¥", "₩"," ₩₩", "€" ,"€€", "฿ ","฿฿", )) %>%
  filter(Award %ni% c("Bib Gourmand", "Bib Gourmand,Green Star", "Green Star")) %>%
  filter(Location != "São Paulo, Brazil") %>%
  filter(distancia <= 2000) %>%
  select(Name, distancia, Award, Price)

#Distância mínima para a culinária Creative
dados %>%
  group_by(Cuisine) %>%
  filter(str_detect(Cuisine, "Creative")) %>%
  filter(Location != "São Paulo, Brazil") %>%
  arrange(distancia) %>%
  select(Name, distancia, Cuisine)


##############################################################
dados %>%
  filter(Name == "Le Chat-Botté")

#Km para restaurente 1 Star
dados %>%
  group_by(Award) %>%
  filter(Award %in% c("1 Star", "1 Star,Green Star")) %>%
  arrange(distancia) %>%
  filter(Location != "Geneva, Switzerland") %>%
  select(Name, distancia)

#Quantos restaurantes com 1,2 ou 3 Star em até 100km
dados %>%
  group_by(Award) %>%
  filter(Award %in% c("3 Stars,Green Star", "3 Stars", "2 Stars", "2 Stars,Green Star", "1 Star", "1 Star,Green Star")) %>%
  filter(distancia <= 100) %>%
  select(Name, distancia)

#Até 4 dinheiros, fora de geneva, até 2000km, pelo menos 1 estrela
dados%>%
  group_by(Award) %>%
  filter(Award %ni% c("Bib Gourmand", "Bib Gourmand,Green Star", "Green Star")) %>%
  filter(Location != "Geneva, Switzerland") %>%
  filter(distancia <= 2000) %>%
  nrow()

#Culinária Farm to Table
dados %>%
  group_by(Cuisine) %>%
  filter(str_detect(Cuisine, "Farm to table")) %>%
  filter(Location != "Geneva, Switzerland") %>%
  arrange(distancia)%>%
  select(Name, distancia)





