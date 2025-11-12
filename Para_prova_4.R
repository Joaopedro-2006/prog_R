##Modulo 4

#importando arquivos de texto pleno

## Importando arquivo do tipo .txt
url <- "http://leg.ufpr.br/~wagner/scientificR/anovareg.txt"
dados <- read_tsv(url, col_names = TRUE) ->#col_names = TRUE indica que as colunas
  head(dados)                                # tem nome

## Importando dados do tipo .csv
url <- "http://leg.ufpr.br/~wagner/scientificR/reglinear.csv"
dados <- read_table(url, col_names = TRUE)
head(dados)

## Baixando e importando uma planilha eletrônica
# Se precisar instalar o readxl ou o httr descomente os códigos abaixo
# install.packages("readxl")
# install.packages("httr")

library(readxl)
library(httr)
url <- "http://leg.ufpr.br/~wagner/scientificR/meus_dados.xlsx"
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
tb <- read_excel(tf, sheet = "mtcars")
View(tb)

## Conexão com banco de dados relacionais
## O banco usado para exemplo é público e pode sair do ar.

## Se não tiver instalado os pacotes DBI e RMySQL descomente as linhas abaixo
# install.packages("DBI")
# install.packages("RMySQL")

library(DBI)
library(RMySQL)

# Criando a conexão.
db <- dbConnect(
  RMySQL::MySQL(),
  user = "rfamro", password = "",
  port = 4497, dbname = "Rfam",
  host = "mysql-rfam-public.ebi.ac.uk")

# Lista as tabelas do BD.
dbListTables(db)

# Listas as colunas em uma tabela.
dbListFields(db, "keywords")

# Importanto a tabela.
tb <- RMySQL::dbFetch(
  RMySQL::dbSendQuery(
    db, "SELECT * FROM keywords"))
str(tb)
View(tb)
# Desconecta
dbDisconnect(db)


#APIs
#como funciona? Vc manda uma requisição para a API -> O serviço (dados/modelos/etc) 
#processa o pedido e gera uma resposta -> A API devolve a resposta pra vc.
#Permitem: automação, integração e acesso em tempo real a informações externas.

## Carregando pacotes adicionais
library(httr)
library(jsonlite)
library(dplyr)

# -----------------------------
# 1. API do IBGE - Lista de Estados do Brasil
# Documentação: https://servicodados.ibge.gov.br/api/docs/
# -----------------------------
res_ibge <- GET("https://servicodados.ibge.gov.br/api/v1/localidades/estados")

# Verificando status da resposta
stop_for_status(res_ibge)

# Conteúdo da resposta (parsed = já em lista R)
estados <- content(res_ibge, as = "parsed", encoding = "UTF-8")
estados

# Organizando para ter um data.frame
df_estados <- tibble::tibble(
  id = sapply(estados, `[[`, "id"),
  nome = sapply(estados, `[[`, "nome"),
  sigla = sapply(estados, `[[`, "sigla"),
  regiao = sapply(estados, function(x) x$regiao$nome)
)

head(df_estados)
View(df_estados)


##Setup pipe 
install.packages("readr")
require(tidyverse)

dados <- readr::read_csv("Mental Health Dataset.csv")
## Para vermos os dados, podemos utilizar a função head()
head(dados, 2)
glimpse(dados)



#O operador %>%

#É chamado de pipe e é utilizado para encadear funções.

x <- c(-2:2)
x

# Opção 1 - Sem identação
sort(cos(unique(x)), decreasing = TRUE)

# Opção 2 - Com identação
sort(
  cos(
    unique(
      x
    )
  ), 
  decreasing = TRUE)


# Opção 3 - Utilizando o operador %>%
require(magrittr)

x %>% 
  unique() %>% 
  cos() %>%
  sort(decreasing = TRUE)

y = x %>% 
  unique() %>% 
  cos() %>%
  sort(decreasing = TRUE)
y

## Pipe de atribuição %<>%
x <- 1:10
x 
x %<>% log()
x


##### Manipulação de dados com dplyr
## Criar Colunas com Mutate

install.packages("dados")
require(dplyr)
require(magrittr)
require(tidyverse)

dados <- dados %>% 
  mutate(mercosul = ifelse(Country %in%
                             c("Argentina", "Brazil", "Paraguay", "Uruguay"),
                           "Mercosul", "Não Mercosul"))
glimpse(dados)
table(dados$mercosul)

##SELECT SÃO SEMPRE NAS COLUNAS
# Selecionar colunas com select()

dados2 <- dados %>% 
  select(Country, Timestamp, Days_Indoors, mercosul)
glimpse(dados2)

##Selicionar variaveis pela posicao
dados3 <- dados %>% 
  select(3:5)
glimpse(dados3)

##selecionar as variaveis pelo nome em um intervalo de variaveis
dados4 <- dados %>% 
  select(treatment:Changes_Habits)
glimpse(dados4)

##selecionar variaveis por meio de padroes(Letras, )
dados6 <- dados %>% 
  select(starts_with("t"))
glimpse(dados6)

dados7 <- dados %>% 
  select(ends_with("s"))
glimpse(dados7)

dados %>% 
  select(ends_with("ss")) %>% 
  glimpse()

dados8 <- dados %>% 
  select(contains("ing"))
glimpse(dados8)

dados9 <- dados %>% 
  select(matches("[tT]"))
glimpse(dados9)

dados10 <- dados %>% 
  select(-Country, -Timestamp, -Days_Indoors, -mercosul)
glimpse(dados10)

dados11 <- dados %>% 
  select(-c(Country, Timestamp, Days_Indoors, mercosul))
glimpse(dados11)

dados12 <- dados %>% 
  select_if(is.character)
glimpse(dados12)

dados %>% 
  select_if(is.logical)

dados %>% 
  select_if(is.numeric)

variaveis <- c("Country", "Timestamp", "Days_Indoors", "mercosul")
dados13 <- dados %>% 
  select(all_of(variaveis)) # Em all_of,todas as variaveis devem existir, se 
glimpse(dados13)            # uma n existir no banco de dados, da erro

variaveis <- c("Country", "Timestamp", "Days_Indoors", "mercosul", "Futebol")
dados14 <- dados %>% 
  select(any_of(variaveis)) #Em any_of, seleciona apenas as variaveis que existem 
glimpse(dados14)            #no banco de dados, mesmo que tenha uma que n exista 

dados15 <- dados %>% 
  filter(mercosul == "Mercosul")

table(dados15$mercosul)

# Carregando dados comprimidos (.gz)
require(dplyr)
require(magrittr)
require(tidyverse)

library(data.table)
car_crash <- fread("Brazil Total highway crashes 2010 - 2023.csv")

glimpse(car_crash)

##FILTER SÃO SEMPRE NAS LINHAS
# Filtrando linhas com filter()
car_crash2 <- car_crash %>% 
  filter(tipo_de_ocorrencia == "sem vítima")
glimpse(car_crash2)

## Filtros com múltiplas condições
car_crash3 <- car_crash %>% 
  filter(tipo_de_ocorrencia == "sem vítima" & automovel >= 3)
glimpse(car_crash3)

car_crash4 <- car_crash %>% 
  filter(between(automovel, 3, 5))
glimpse(car_crash4)

tipos <- c("sem vítima", "com vítima")
tipos
car_crash5 <- car_crash %>% 
  filter(tipo_de_ocorrencia %in% tipos)
car_crash5

car_crash6 <- car_crash %>% 
  filter(!tipo_de_ocorrencia %in% c("sem vítima", "com vítima"))
glimpse(car_crash6)

## Usando o operador %ni%
`%ni%` <- Negate(`%in%`)  
car_crash7 <- car_crash %>%
  filter(tipo_de_ocorrencia %ni% c("sem vítima", "com vítima"))
glimpse(car_crash7)

## Operador like %like%
car_crash8 <- car_crash %>% 
  filter(tipo_de_ocorrencia %like% "vítima") 
glimpse(car_crash8)  #%like serve para achar observaçoes que tenham tal palavra

car_crash9 =  car_crash %>% 
  filter(grepl("ilesa|fatal", tipo_de_ocorrencia))
glimpse(car_crash9)


## Ordenando linhas com arrange()
car_crash10 = car_crash %>% 
  arrange(desc(automovel))
glimpse(car_crash10)

car_crash11 = car_crash %>% 
  arrange(desc(automovel), mortos) %>%
  select(automovel, mortos) %>% 
  na.exclude()
head(car_crash11)

car_crash_slice1 = car_crash %>% 
  select(1:5) %>%
  slice(3:5)
car_crash_slice1

car_crash_slice2 = car_crash %>% 
  select(1:5) %>%
  slice_head(n = 3)
car_crash_slice2


car_crash_slice3 = car_crash %>% 
  select(1:5) %>%
  slice_tail(n = 3)
car_crash_slice3


##EXERCICIO 1

# 1. Filtre as observações cujo tipo de evento é Tropical Depression . 
#Quantas observações existem? 

storms %>% 
  filter(status == "tropical depression") %>% 
  nrow()

#2. Filtre as observações cujo tipo de evento é Tropical Depression e a velocidade 
#do vento é maior ou igual a 40. Quantas observações existem?

storms %>%
  filter(status == "tropical depression" & wind >= 30) %>% 
  nrow()

# 3. Selecione as variáveis numéricas e ordene as observações pela variável pressure 
# em ordem crescente.

storms %>%
  select_if(is.numeric) %>% 
  arrange(pressure)


# Rename
car_carsh12 = car_crash %>% 
  rename(numero_automoveis = automovel)
glimpse(car_carsh12)


# Relocate
car_crash_relocate = car_crash %>% 
  relocate(automovel, .before = 1)
glimpse(car_crash_relocate)

car_crash_relocate2 = car_crash %>% 
  relocate(mortos, .after = last_col())
glimpse(car_crash_relocate2)

# Transmute
car_crash_transmute <- car_crash %>% 
  transmute(automovel_10 = automovel / 10)
glimpse(car_crash_transmute)

# NA Replace
car_crash_replace_na <- car_crash %>%
  mutate(mortos = replace_na(mortos, 0))
glimpse(car_crash_replace_na)


# Cut
car_crash_cut <- car_crash %>%
  mutate(automovel = replace_na(automovel, 0)) %>%
  mutate(automovel_cat = cut(automovel,
                             breaks = c(-Inf, 0, 3, Inf),
                             labels = c("sem automóveis",
                                        "entre 1 e 3 automóveis",
                                        "mais do que três")))
glimpse(car_crash_cut)
table( car_crash_cut$automovel, 
       car_crash_cut$automovel_cat)

## Summarise
car_crash13 = car_crash %>% 
  summarise(total_automoveis = sum(automovel, na.rm = TRUE))
car_crash13

sum(car_crash$automovel, na.rm = T)

car_crash14 = car_crash %>% 
  summarise(total_automoveis = sum(automovel, na.rm = TRUE),
            total_mortos = sum(mortos, na.rm = TRUE), 
            n = n(), 
            media_mortos = mean(mortos, na.rm = TRUE))
car_crash14
sum(car_crash$mortos, na.rm = T)
nrow(car_crash)
mean(car_crash$mortos, na.rm = T)


## Agrupamento com group_by()
require(lubridate)

car_crash15 = car_crash %>% 
  mutate(ano = year(dmy(data))) %>%
  group_by(ano)
glimpse(car_crash15)

car_crash16 = car_crash %>% 
  mutate(ano = year(dmy(data))) %>%
  group_by(ano) %>%
  summarise(total_automoveis = sum(automovel, na.rm = TRUE),
            total_mortos = sum(mortos, na.rm = TRUE))
head(car_crash16)

car_crash17 = car_crash %>% 
  filter(tipo_de_ocorrencia == "com vítima") %>%
  summarise(total_automoveis = sum(automovel, na.rm = TRUE),
            total_mortos = sum(mortos, na.rm = TRUE))
car_crash17



## Exercícios 

# 1. Qual é o número total de espécies únicas presentes? 

starwars %>%
  summarise(n_especies = n_distinct(species))

#Qual a frequência de indivíduos por espécie?

starwars %>%
  group_by(species) %>%
  summarise(freq_especies = n()) %>%
  arrange(desc(freq_especies))

# 2.Calcule a altura média de personagens masculinos e femininos

starwars %>% 
  filter(sex %in% c("female", "male")) %>% 
  group_by(sex) %>%
  summarise(media_altura = mean(height, na.rm = TRUE))

# 3.Qual é o peso médio dos personagens de cada espécie para personagens masculinos?

starwars %>% 
  filter(sex == "male") %>%
  group_by(species) %>%
  summarise(media_peso = mean(mass, na.rm = TRUE))

# 4. Para cada espécie presente na base de dados, identifique o personagem mais 
#pesado e seu peso correspondente.

starwars %>% 
  group_by(species) %>%
  filter(mass == max(mass, na.rm = TRUE)) %>%
  select(species, name, mass)


## Trabalhando com datas - lubridate

car_crash %>% 
  mutate(data = dmy(data)) %>%
  select(data) %>%
  glimpse()

car_crash %>% 
  mutate(data = dmy(data)) %>%
  mutate(ano = year(data),
         mes = month(data),
         dia = day(data)) %>%
  select(data, ano, mes, dia) %>%
  glimpse()

car_crash %>% 
  mutate(data = dmy(data)) %>%
  mutate(dias_desde_acidente = difftime(Sys.Date(), data, units = "days")) %>%
  select(data, dias_desde_acidente) %>%
  head()

car_crash %>% 
  mutate(data = dmy(data)) %>%
  mutate(data_mais_10_dias = data + lubridate::days(10)) %>%
  select(data, data_mais_10_dias) %>%
  head()



## Extraindo componentes de data e hora

data <- ymd_hms("2023-08-21 15:30:45")
ano <- year(data)
mes <- month(data)
dia <- day(data)
hora <- hour(data)
minuto <- minute(data)
segundo <- second(data)

print(ano)
print(mes)
print(dia)
print(hora)
print(minuto)
print(segundo)

# Data original no fuso horário de Nova Iorque
data_ny <- ymd_hms("2025-10-21 12:00:00", tz = "America/New_York")

# Converter para o fuso horário de Londres
data_london <- with_tz(data_ny, tz = "Europe/London")

print(data_ny)
print(data_london)


## Exercícios com datas

# 1. Quais os meses do ano com maior número de acidentes fatais?

car_crash %>% 
  mutate(data = dmy(data)) %>%
  mutate(ano = year(data),
         mes = month(data)) %>%
  select(data, ano, mes, mortos) %>%
  filter(mortos > 0) %>% 
  group_by(mes) %>%
  summarise(total_mortos = sum(mortos)) %>% 
  arrange(desc(total_mortos))

# 2. Quais os dias da semana com maior número de acidentes fatais?

car_crash %>% 
  mutate(data = dmy(data)) %>%
  mutate(dia_semana = lubridate::wday(data, label = T, abbr = F)) %>%
  select(dia_semana, mortos) %>%
  filter(mortos > 0) %>% 
  group_by(dia_semana) %>%
  summarise(total_mortos_dia = sum(mortos)) %>% 
  arrange(desc(total_mortos_dia))


# Tidy data

# Carregar pacotes necessários
library(tidyverse)

# Visualizar o dataset table1
table1

# Transformar dados long -> wide usando pivot_wider()
table1 %>% 
  select(-population) %>%
  pivot_wider(names_from = year, 
              values_from = cases)

# Pivotar com mais de uma variável
table1 %>% 
  pivot_wider(names_from = year, 
              values_from = c(cases, population))

# Transformar dados wide -> long usando pivot_longer()
table1 %>% 
  pivot_longer(cols = c(cases, population), 
               names_to = "variable", 
               values_to = "total")


# Separando e juntando observações


# Visualizar o dataset table3
table3

# Separar coluna 'rate' em 'cases' e 'population'
table3 %>% 
  separate(rate, into = c("cases", "population"))

# Juntar colunas 'cases' e 'population' em uma única coluna 'rate'
table1 %>% 
  unite(rate, cases, population, sep = "/")

##Exercicios

#1. Utilizando os dados de flights , do pacote {nycflights13} , crie uma matriz 
#que mostra o número de voos entre cada par de aeroportos.

# Utilizando os dados do pacote nycflights13

require(nycflights13)

flights %>% 
  count(origin, dest) %>% 
  pivot_wider(names_from = origin, 
              values_from = n, 
              values_fill = 0)



#Funções Básicas do {stringr}

#str_length() : Retorna o comprimento de uma string.
#str_to_lower() : Converte uma string para minúsculas.
#str_to_upper() : Converte uma string para maiúsculas.
#str_sub() : Extrai uma substring de uma string.
#str_replace() : Substitui uma parte de uma string por outra.
#str_detect() : Verifica se uma string contém um padrão específico.


# Manipulação de Strings em R

library(stringr)
texto <- "Olá, Mundo!"

# Comprimento da string
str_length(texto)

# Converter para minúsculas
str_to_lower(texto)

# Converter para maiúsculas
str_to_upper(texto)

str_to_title(texto)

str_to_sentence(texto)

# Extrair substring
str_sub(texto, 1, 3)

# Substituir parte da string
str_replace(texto, "Mundo", "R")

# Verificar se a string contém um padrão
str_detect(texto, "Mundo")
str_detect(texto, "R!")


# Expressões Regulares (Regex)

# Corresponder qualquer caractere
str_detect("abc", "a.c") # TRUE

# Início da string
str_detect("abc", "^a")   # TRUE

# Fim da string
str_detect("abc", "c$")   # TRUE

# Zero ou mais ocorrências
str_detect("aaab", "a*b") # TRUE

# Uma ou mais ocorrências
str_detect("aaab", "a+b") # TRUE

# Conjunto de caracteres: corresponde a 'a', 'b' ou 'c'
str_detect("abc", "[abc]") # TRUE

# Operador "ou": corresponde a 'cat' ou 'dog'
str_detect("I have a cat", "cat|dog") # TRUE



## Manipulação de dados ########################################################
################################################################################

## Criando um tibble
# Tabela com alunos do curso de Matemática e de Estatística.
df1 <- tibble(
  mat = c(256, 487, 965,
          125, 458, 874, 963),
  nome = c("João", "Vanessa", "Tiago",
           "Luana", "Gisele", "Pedro",
           "André"),
  curso = c("Mat", "Mat", "Est", "Est",
            "Est", "Mat", "Est"),
  prova1 = c(80, 75, 95, 70, 45, 55, 30),
  prova2 = c(90, 75, 80, 85, 50, 75, NA),
  prova3 = c(80, 75, 75, 50, NA, 90, 30),
  faltas = c(4, 4, 0, 8, 16, 0, 20))

# Informações de cadastro dos alunos em outra base de dados.
df_extra <- tribble(
  ~mat,     ~nome, ~idade, ~bolsista,
  256,  'João'  ,     18,       "S",
  965,  'Tiago' ,     18,       "N",
  285,  'Tiago' ,     22,       "N",
  125,  'Luana' ,     21,       "S",
  874,  'Pedro' ,     19,       "N",
  321,  'Mia'   ,     18,       "N",
  669,  'Luana' ,     19,       "S",
  967,  'André' ,     20,       "N",
)

################################################################################
## Combinação de dados #########################################################
################################################################################

# Concatenação na vertical (pilha).
bind_rows(df1[1:3, c(1, 3, 5)],
          df1[5:7, c(1, 3, 5, 4)],
          df1[4,   c(1,    5, 4)])

# Concatenação na horizontal (fila).
bind_cols(df1[, c(1:3)],
          df1[, c(6:7)])

# Full join = união.
full_join(df1, df_extra,
          by = c("mat" = "mat", "nome"))

# Inner join = intersecção.
inner_join(df1,
           df_extra,
           by = c("mat" = "mat",
                  "nome"))

# Todos os que estão na 1º tabela
left_join(df1, df_extra,
          by = c("mat" = "mat",
                 "nome"))

# Todos os que estão na 2º tabela
right_join(df1, df_extra,
           by = c("mat" = "mat",
                  "nome"))

# Os da 2º que não aparecem na 1º.
anti_join(df1, df_extra,
          by = c("mat" = "mat",
                 "nome"))

################################################################################
## Exportação de dados #########################################################
################################################################################

## Texto pleno
write_csv(df1, 
          file = "Nome_do_arquivo.csv")

## Binário padrão R
save(df1, file = "Nome_do_arquivo.RData")

## Carregando arquivo .RData
load("Nome_do_arquivo.RData")

## Planilha eletrônica
library(writexl)
write_xlsx(df1, "Nome_do_arquivo.xlsx")


