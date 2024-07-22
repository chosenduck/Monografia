# Limpando a memória e todos os objetos atualmente ativos ======================
rm(list=ls(all=T))
gc()

# Bibliotecas ==================================================================
pacman::p_load(tidyverse, stringr, lubridate, writexl, readr, knitr,
               janitor, openxlsx, skimr, ggplot2, rvest, stopwords, stringi,
               stringr, abjutils, tm, tidytext, english, wordcloud, quanteda,
               stm, textclean, reshape2)

# Ajustar a referência da pasta dos dados ======================================
getwd()
setwd("")

# Bases ========================================================================
revisionismo <- read.csv("videoinfo_zBFd-XIzu0k_2024_07_11-19_39_44_comments.csv")

glimpse(revisionismo)

erikahilton <- read.csv("videoinfo_Zmbl9IC7Rnk_2024_07_11-19_54_07_comments.csv")

glimpse(erikahilton)

### Stopwords ==================================================================
sw_ptbr <- read.csv("stopwords_ptbr.txt") 
glimpse (sw_ptbr)

sw_ptbr <- sw_ptbr %>%
  mutate(X0 = str_to_lower(X0),
         X0 = removePunctuation(X0),
         X0 = rm_accent(X0)) %>% 
  rename(text = X0)

sw_ptbr <- sw_ptbr$text

# Tratamento dos comentários ===================================================
### Funções ====================================================================
# Função para remover stopwords
remove_stopwords <- function(text) {
  words <- unlist(str_split(text, "\\s+"))  # Divide o texto em palavras
  filtered_words <- words[!words %in% sw_ptbr]  # Filtra as stopwords
  paste(filtered_words, collapse = " ")  # Junta as palavras de volta em uma string
}

# Função para remover url
remover_url <- function(text) {
  padrao_url <- "https?://[^\\s]+"
  texto_sem_url <- gsub(padrao_url, "", text)
  return(texto_sem_url)
}

### Limpeza de comentarios =====================================================
#### Revisionismo ==============================================================
revisionismo_c <- revisionismo %>% 
  select(authorName, text, likeCount) %>% 
  mutate(text = str_to_lower(text),
         text = remover_url(text),
         text = removeNumbers(text),
         text = removePunctuation(text),
         text = rm_accent(text)
         ) %>% 
  filter(likeCount >5) %>% 
  unnest_tokens(palavras, text) %>% 
  filter(!palavras %in% sw_ptbr)

revisionismo_c %>% 
  count(palavras, sort = TRUE)

palavra <- "hist" # substituir aqui a palavra ou padrão desejados

ocorrencias_filtradas <- revisionismo_c %>%
  filter(str_detect(palavras, fixed(palavra)))

# Contar a frequência de cada ocorrência única
ocorrencias_contagem <- ocorrencias_filtradas %>%
  group_by(palavras) %>%
  summarise(frequencia = n()) %>%
  ungroup()

# Ordenar as ocorrências pela frequência em ordem decrescente e selecionar as 
# 25 primeiras
ocorrencias_top25 <- ocorrencias_contagem %>%
  arrange(desc(frequencia)) %>%
  slice_head(n = 25)

print(ocorrencias_top25, n = 25)

substituicoes_rev <- c("brasileiros" = "brasileiro",
                       "portugueses" = "portugues",
                       "portuguesa" = "portugues",
                       "indigenas" = "indigena",
                       "indio" = "indigena",
                       "universidades" = "universidade",
                       "colonias" = "colonia",
                       "colonizada" = "colonizado",
                       "colonizados" = "colonizado",
                       "colonizar" = "colonizacao",
                       "colonizaram" = "colonizacao",
                       "colonisadores" = "colonizadores",
                       'colonizacoes' = "colonizacao",
                       "colonizacaomas" = 'colonizacao',
                       "historiadores" = 'historiador',
                       "historica" = "historia",
                       "historico" = "historia",
                       "historias" = "historia",
                       "historiasabe" = 'historia',
                       "historiate" = 'historia',
                       "historiografiaparabens"= "historia")

remover_rev <- c("algumnome", "etc", "leidsonreis", "unknownman", "vygm", 
                 "jeysondacostaoliveira", "blablabla", "bylazerions", "juantomaz",
                 "ludwigwittgenstein", "zorro", "leonardovmgdobrigado")  

revisionismo_c <- revisionismo_c %>%
  mutate(palavras = str_replace_all(palavras, substituicoes_rev)) %>% 
  filter(!palavras %in% remover_rev)

#### Erika Hilton ==============================================================
erikahilton_c <- erikahilton %>% 
  select(authorName, text, likeCount) %>% 
  mutate(text = str_to_lower(text),
         text = remover_url(text),
         text = removeNumbers(text),
         text = removePunctuation(text),
         text = rm_accent(text)
  ) %>%
  filter(likeCount > 10) %>% 
  unnest_tokens(palavras, text) %>% 
  filter(!palavras %in% sw_ptbr)

erikahilton_c %>% 
  count(palavras, sort = TRUE)

palavra <- "eri" # substituir aqui a palavra ou padrão desejados

ocorrencias_filtradas <- erikahilton_c %>%
  filter(str_detect(palavras, fixed(palavra)))

# Contar a frequência de cada ocorrência única
ocorrencias_contagem <- ocorrencias_filtradas %>%
  group_by(palavras) %>%
  summarise(frequencia = n()) %>%
  ungroup()

# Ordenar as ocorrências pela frequência em ordem decrescente e selecionar as 
# 25 primeiras
ocorrencias_top25 <- ocorrencias_contagem %>%
  arrange(desc(frequencia)) %>%
  slice_head(n = 25)

print(ocorrencias_top25, n = 25)

substituicoes_hilt <- c("mulheres" = "mulher",
                       "hilton" = "erika",
                       "herica" = "erika",
                       "ericka" = "erika",
                       "gays" = "gay",
                       "videos" = "video",
                       "gestam" = "gesta",
                       "misogino" = "misoginia",
                       "passar" = "passa",
                       "renais" = "renan",
                       "pablo" = "vittar",
                       "vittarbomgiorno" = "vittar",
                       "figuras" = "figura",
                       "esteriotipando" = "esteriotipos",
                       "experiencias" = "experiencia",
                       "erikalexsispinosalamanca" = "erika",
                       "erikalexsispinosalamancaprovavelmente" = "erika",
                       "alemas" = "alema"
                       )

substituicoes_hilt_2 <- c("erica" = "erika")
                        

remover_hilt <- c("dai", "etc", "marcusbonetto","nisso","sai", 
                  "antoniocarlosoliveira", "oi", "azul", "ces", "fico")  

erikahilton_c <- erikahilton_c %>%
  mutate(palavras = str_replace_all(palavras, substituicoes_hilt)) %>% 
  filter(!palavras %in% remover_hilt)

erikahilton_c <- erikahilton_c %>%
  mutate(palavras = str_replace_all(palavras, substituicoes_hilt_2))

# Modelagem ====================================================================
# Criando matrizes
revisionismo_m <- revisionismo_c %>% 
  count(authorName, palavras, sort = TRUE) %>% # visualizando palavras mais usadas
  cast_sparse(authorName, palavras, n)

dim(revisionismo_m) # 23 comentadores e 295 palavras

erikahilton_m <- erikahilton_c %>% 
  count(authorName, palavras, sort = TRUE) %>%  # visualizando palavras mais usadas
  cast_sparse(authorName, palavras, n)

dim(erikahilton_m) # 51 comentadores e 392 palavras

# Treinando modelos
?stm

modelo_rev <- stm(revisionismo_m, 
                  K = 6) # supondo um valor ideal para K

summary(modelo_rev)

modelo_hilt <- stm(erikahilton_m, 
                   K = 8) # supondo um valor ideal para K

summary(modelo_hilt)

# Explorando resultados
topicos_palavras_rev <- tidy(modelo_rev, 
                             matrix = "beta") # beta é a probabiblidade de cada palavra ser associada com cada topico
topicos_palavras_rev

topicos_palavras_rev %>%
  group_by(topic) %>%
  slice_max(beta, n = 7, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(topic = paste("Topico", topic)) %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL)


# Explorando resultados
topicos_palavras_hilt <- tidy(modelo_hilt, 
                             matrix = "beta") # beta é a probabiblidade de cada palavra ser associada com cada topico
topicos_palavras_hilt

topicos_palavras_hilt %>%
  group_by(topic) %>%
  slice_max(beta, n = 7, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(topic = paste("Topico", topic)) %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL)