install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load(2021, week = 48)
tuesdata


##### Dr. Who

writers <- tuesdata$writers
directors <- tuesdata$directors
episodes <- tuesdata$episodes
imdb <- tuesdata$imdb

View(directors)
View(episodes)
View(writers)
View(imdb)

#juntando
episodes <- left_join(episodes, directors, by = "story_number")
episodes <- left_join(episodes, writers, by = "story_number")
glimpse(imdb)
glimpse(episodes)
episodes <- episodes %>%
  mutate(
    Director_Writer = paste(director, writer, sep = " / ")
  )
episodes <- episodes %>%
  mutate(ano = year(first_aired))

#Dupla de diretor e escritor Daniel Nettheim e Peter Harness é responsável por quantos episódios
episodes %>%
  filter(writer == "Peter Harness" & director == "Daniel Nettheim") %>%
  select(director, writer, season_number, episode_number)

#Episódios de Steven Moffat e 2011
episodes %>%
  group_by(writer) %>%
  filter(ano == "2011") %>%
  filter(writer %in% "Steven Moffat") %>%
  select(ano, writer, episode_number, season_number)

#Por quantos anos o escritor Jamie Mathieson trabalhou na série
episodes %>%
  group_by(ano)%>%
  filter(writer == "Jamie Mathieson") %>%
  select(ano, writer)

#Quantos episódios o escritor Stephen Thompson fez
episodes %>%
  filter(writer == "Jamie Mathieson") %>%
  select(season_number, episode_number, ano, writer)

#tempo de duração média do diretor Daniel O’Hara
episodes %>%
  group_by(director) %>%
  summarise(duração_média = mean(duration, na.rm = T)) %>%
  filter(director == "Nick Hurran")

#### Dr. Who
#Dupla Daniel Nettheim / Peter Harness
episodes %>%
  group_by(Director_Writer) %>%
  filter(Director_Writer == "Daniel Nettheim / Peter Harness") %>%
  select(Director_Writer, episode_number, season_number)

#Episódios que Russell T Davies escreveu em 2005
episodes %>%
  group_by(ano) %>%
  filter(ano == "2005") %>%
  filter(writer == "Russell T Davies") %>%
  select(writer, episode_number, season_number)

#Quantos anos o escritor Jamie Mathieson trabalhou na série
episodes%>%
  filter(writer == "Jamie Mathieson") %>%
  select(ano, writer, episode_number, season_number)

#Duração média dos episódios de Richard Clark
episodes %>%
  group_by(director) %>%
  filter(director == "Richard Clark") %>%
  summarise(dur.med = mean(duration, na.rm = T))
