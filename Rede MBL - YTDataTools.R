# Limpando a memória e todos os objetos atualmente ativos ======================
rm(list=ls(all=T))
gc()

# Bibliotecas ==================================================================
if (!require(pacman))
  install.packages("pacman")

require(pacman)

pacman::p_load(tidyverse, stringr, lubridate, writexl, readr, janitor, openxlsx, 
               skimr, ggplot2, rvest, stringr, abjutils, tm, tidytext, english, 
               wordcloud2, quanteda, stm, textclean, webshot, htmlwidgets, 
               igraph)

webshot::install_phantomjs()

# Ajustar a referência da pasta dos dados ======================================
getwd()
setwd("")

# Base de canais  ==============================================================
# encontrados a partir da busca "movimento brasil livre"
base <- read.csv("channelsearch_channels464_2024_07_04-17_42_20.csv") 

glimpse(base)

### Filtragem ==================================================================
# apenas canais que contem "mbl" ou "movimento brasil livre" no nome
lista_canais <- base %>% 
  select(position, id, title, description) %>% 
  mutate(nome_canal = str_to_lower(title),
         nome_canal = rm_accent(nome_canal),
         nome_canal = removeNumbers(nome_canal),
         nome_canal = removePunctuation(nome_canal),
         descricao = str_to_lower(description),
         descricao = rm_accent(descricao),
         descricao = removeNumbers(descricao),
         descricao = removePunctuation(descricao))

canais_mbl_n <- lista_canais %>% # filtrando por nome do canal
  filter(str_detect(nome_canal, str_c("mbl", "movimento brasil livre", 
                                      sep = "|"))) 

canais_mbl_d <- lista_canais %>% # filtrando por descrição
  filter(str_detect(description, str_c("mbl", "movimento brasil livre", 
                                      sep = "|")))

canais_mbl <- bind_rows(canais_mbl_n, canais_mbl_d) # unindo ambas

canais_mbl <- canais_mbl %>% # retirando canais que não atendiam aos critérios 
  filter(!position %in% c(10, 111, 249)) %>% 
  left_join(base, by = "id") %>% # puxando o restante dos dados da base
  mutate(criacao = format(ymd_hms(publishedAt), "%Y-%m")) %>% 
  select(-c(position.x, title.x, description.x, title.y, description.y,
            publishedAt, defaultLanguage, country, thumbnail)) %>% 
  select(posicao = position.y, everything())

### Salvando ===================================================================
# write.xlsx(canais_mbl, "")
  
### ID's =======================================================================
# separando id's dos canais para montar a rede no youtube datatools
id_canais_mbl <- canais_mbl %>% 
  pull(id) %>% # extraindo id's
  paste(collapse = ",") # separando elas por vírgula

print(id_canais_mbl)

# Base de Tags =================================================================
tags <- read.csv("tags_mbl.csv", 
                 sep = ",", 
                 fileEncoding = "UTF-8")

glimpse(tags)

length(unique(tags$Source))

### Funções ====================================================================
# Função para converter números em palavras
# datas, e outros valores
convert_num <- function(number) {
  if (number == 0) {
    return("zero")
  }
  return(as.character(as.english(number)))
}

# Função para substituir números por palavras
trocar_num <- function(text) {
  # Encontra todos os números na string
  numbers <- str_extract_all(text, "\\b\\d+\\b")[[1]]
  # Substitui cada número pela sua representação em palavras
  for (number in numbers) {
    word <- convert_num(as.numeric(number))
    text <- str_replace_all(text, fixed(number), word)
  }
  return(text)
}

### Padronização ===============================================================
# padronizando tags
tags_mod <- tags %>% 
  mutate(Source = rm_accent(Source),
         Source = trocar_num(Source),
         Source = str_to_lower(Source),
         Source = removePunctuation(Source),
         Target = rm_accent(Target),
         Target = trocar_num(Target),
         Target = str_to_lower(Target),
         Target = removePunctuation(Target))

tags_mod2 <- tags_mod %>% 
  select(Source)

tags_mod2 %>% 
  count(Source) %>%
  arrange(-n) %>%
  filter(n >= 50)

### Filtragem  =================================================================
palavra <- "trans" # substituir aqui a palavra ou padrão desejados

ocorrencias_filtradas <- tags_mod2 %>%
  filter(str_detect(Source, fixed(palavra)))

# Contar a frequência de cada ocorrência única
ocorrencias_contagem <- ocorrencias_filtradas %>%
  group_by(Source) %>%
  summarise(frequencia = n()) %>%
  ungroup()

# Ordenar as ocorrências pela frequência em ordem decrescente e selecionar as 
# 25 primeiras
ocorrencias_top25 <- ocorrencias_contagem %>%
  arrange(desc(frequencia)) %>%
  slice_head(n = 25)

print(ocorrencias_top25, n = 25)

### Substituições ==============================================================
# vetor de substituições
substituicoes <- c(
  "dia 8 de janeiro" = "08/01",
  "8 de janeiro" = "08/01",
  "8 de janiero" = "08/01",
  "dia 8" = "08/01",
  
  "11 de setembro" = "11/09",
  "7desetembro"= "07/09",
  
  "24 de feveriro" = "24/02",
  
  "26 05" = "26/05",
  
  "3 em 1" = "3em1",
  
  "abordagem com tiros" = "abordagem policial",
  
  "mbl x ancaps" = "ancap",
  "ancapistao" = "ancap",
  "ancapismo" = "ancap",
  
  "libertadores" = "libertario",
  "libertarianismo" = "libertario",
  "libertarian" = "libertario",
  "libertario" = "libertario",
  "rumbo libertad" = "libertario",
  "libertarios" = "libertario",
  
  "imposto" = "imposto",
  "mais imposto" = "imposto",
  "imposto do amor" = "imposto",
  "imposto do lula" = "imposto",
  "lula e imposto" = "imposto",
  "imposto do haddad" = "imposto",
  "imposto de renda" = "imposto",
  "china sites imposto" = "imposto",
  "imposto shopee" = "imposto",
  "imposto aliexpress" = "imposto",
  "imposto sindical" = "imposto",
  "imposto vai aumentar" = "imposto",
  "impostos" = "imposto",
  "haddad reduz impostos de caminhoes" = "imposto",
  "aumento de impostos" = "imposto",
  "paulo guedes impostos" = "imposto",
  "mbl imposto" = "imposto",
  "menosimpostos" = "imposto",
  "imposto e roubo" = "imposto",
  "fim do imposto sindical" = "imposto",
  
  "henry bugalho acusa mbl de naz1sm0" = "nazismo",
  "mbl e nazi" = "nazismo",
  "bolsonaro nazista" = "nazismo",
  "nazismo" = "nazismo",
  "alvim o nazista" = "nazismo",
  "nazista" = "nazismo",
  "nazismo e fascismo" = "nazismo",
  "bolsonazi" = "nazismo",
  "bozonazi" = "nazismo",
  "ciro chama sulistas de nazistas" = "nazismo",
  
  "aecio neves" = "aecio",
  "luciana genro e aecio neves" = "aecio",
  
  "geraldo alckmin" = "alckmin",
  "geraldo alkimin" = "alckmin",
  "propaganda geraldo" = "alckmin",
  "propaganda geraldo alckmin" = "alckmin",
  
  "lula e alexandre de moraes" = "alexandre de moraes",
  "alexandre de moraes da ultimato em bolsonaro" = "alexandre de moraes",
  "alexandre de moares" = "alexandre de moraes",
  "alexandre de morais" = "alexandre de moraes",
  "alexandre de moraes e andre mendonca" = "alexandre de moraes",
  "alexandre de moraes e advogado" = "alexandre de moraes",
  "stf alexandre" = "alexandre de moraes",
  "alexandre de moraes bolsonaro passaporte" = "alexandre de moraes",
  "alexandre de mores" = "alexandre de moraes",
  "grampo em alexandre de moraes" = "alexandre de moraes",
  "mbl fala abertamente sobre alexandre de moraes" = "alexandre de moraes",
  "alexandre de moraes aeroporto" = "alexandre de moraes",
  "imprensa alexandre de moraes aeroporto agredido vs bettega agredido na ufsc" = "alexandre de moraes",
  "alexandre vs onark" = "alexandre de moraes",
  "alexandre borges" = "alexandre de moraes",
  "alexandre garcia" = "alexandre de moraes",
  
  "allan dos panos" = "allan",
  "allan do santos" = "allan",
  "allan dos santos preso" = "allan",
  "allan dos santos nando moura" = "allan",
  "allan dos santos e bolsonaro" = "allan",
  
  "amanda vettorazzo agredida" = "amanda vettorazzo",
  "amanda vetorazzo" = "amanda vettorazzo",
  "santinele mente sobre amanda" = "amanda vettorazzo",
  "amanda vetorrazzo" = "amanda vettorazzo",
  "amanda mbl" = "amanda vettorazzo",
  "invadiram a casa da amanda" = "amanda vettorazzo",
  "casa da amanda" = "amanda vettorazzo",
  "mae da amanda" = "amanda vettorazzo",
  "caso amanda" = "amanda vettorazzo",
  "amanda vetorazo" = "amanda vettorazzo",
  
  "joao amoedo" = "amoedo",
  "amoedo novo" = "amoedo",
  "amoedo presidente" = "amoedo",
  "oamoedovemai" = "amoedo",
  "amoedo 2022" = "amoedo",
  "amoedo vai se candidatar" = "amoedo",
  
  "analise renal" = "analises renais",
  "analisesrenais" = "analises renais",
  
  "andre marinho treta" = "andre marinho",
  "adre marinho" = "andre marinho",
  
  "andre marinho show" = "andre marinho",
  "andre marinho e bolsonaro" = "andre marinho",
  "adre marinho" = "andre marinho",
  "andre marinho vs bolsonaro" = "andre marinho",
  "andre marinho bolsonaro" = "andre marinho",
  "andremarinho" = "andre marinho",
  "andre marinho treta" = "andre marinho",
  "andre marinho demitido" = "andre marinho",
  "andre marinho panico" = "andre marinho",
  "andre marinho janta bolsonaro" = "andre marinho",
  "andre marinho no panico" = "andre marinho",
  "andre marinho e tome" = "andre marinho",
  "entrevista andre marinho" = "andre marinho",
  "andre marinho imitacoes" = "andre marinho",
  "andre marinho imitacao" = "andre marinho",
  
  "marielle franco" = "marielle",
  "anielle franco" = "anielle",
  "anielle franco racismo ambiental" = "anielle",
  "aniele franco" = "anielle",
  "a assessora da ministra anielle franco" = "anielle",
  "assessora de anielle franco" = "anielle",
  "mariele franco" = "marielle",
  "acharam quem mandou mat4r marielle" = "marielle",
  
  "governo quer prender arthur do val por questionar" = "arthur do val",
  "arthur do val marajo" = "arthur do val",
  "governo quer prender arthur do val" = "arthur do val",
  "mp investiga arthur do val" = "arthur do val",
  "governo lula parte pra cima de arthur do val" = "arthur do val",
  "investigacao arthur do val" = "arthur do val",
  "arthur mbl" = "arthur do val",
  "arthur ucrania" = "arthur do val",
  "audio arthur do val" = "arthur do val",
  "arthur do val e as refugiadas" = "arthur do val",
  "debate arthur do val" = "arthur do val",
  "arthurdoval" = "arthur do val",
  "maefalei arthur do val" = "arthur do val",
  "cassacao arthur" = "arthur do val",
  "arthur cassado" = "arthur do val",
  "arthur ucranianas" = "arthur do val",
  "arthur audios vazados" = "arthur do val",
  "cassado arthur do val" = "arthur do val",
  "arthur cassacao" = "arthur do val",
  "renan e arthur do val" = "arthur do val",
  "arthurmbl" = "arthur do val",
  "arthur governador" = "arthur do val",
  "moro e arthur do val" = "arthur do val",
  "arthur do val entrevista" = "arthur do val",
  "entrevista arthur do val" = "arthur do val",
  "arthur do val governador" = "arthur do val",
  "arthur na manifestacao" = "arthur do val",
  "arthur do val na paulista" = "arthur do val",
  "arthur bolsopetismo" = "arthur do val",
  "arthur mamaefalei" = "arthur do val",
  "arthur mamae falei" = "arthur do val",
  "arthur do mamae falei" = "arthur do val",
  "arthur moledo" = "arthur do val",
  "arthur moledo do val" = "arthur do val",
  "arthur do mamaefalei" = "arthur do val",
  "mamae falei" = "arthur do val",
  "mamae falei vc caue" = "arthur do val",
  "caue vs mamae falei" = "arthur do val",
  "mamaefalei" = "arthur do val",
  "react gaiofato vs mamae falei" = "arthur do val",
  "react luide mamae falei" = "arthur do val",
  "mamae falei ucrania" = "arthur do val",
  "mamae jantei" = "arthur do val",
  "mamae falei vs orlando silva" = "arthur do val",
  "debate mamaefalei vs orlando" = "arthur do val",
  "mamae falei cassado" = "arthur do val",
  "cassacao mamae falei" = "arthur do val",
  "audios mamae falei" = "arthur do val",
  "deputado mamae falei" = "arthur do val",
  "mamae falei governador" = "arthur do val",
  "mamaefalei governador" = "arthur do val",
  "mamaefalei manifestacao" = "arthur do val",
  "mamaefalei apanhando" = "arthur do val",
  "mamaefalei entrevista" = "arthur do val",
  "entrevista mamae falei" = "arthur do val",
  "mamaefalei twitter" = "arthur do val",
  "rompemos com mamaefalei" = "arthur do val",
  "mamaefalei bolsopetismo" = "arthur do val",
  "arthur mamaefalei" = "arthur do val",
  "ciro e mamaefalei" = "arthur do val",
  "resposta mamaefalei" = "arthur do val",
  "mamaefalei prefeito" = "arthur do val",
  "mamaefalei responde" = "arthur do val",
  "mamaefalei treta" = "arthur do val",
  "ciro mamae falei" = "arthur do val",
  "arthur mamae falei" = "arthur do val",
  "arthur do mamae falei" = "arthur do val",
  "kim e mamaefalei" = "arthur do val",
  "arthur do mamaefalei" = "arthur do val",
  "mamae" = "arthur do val",
  
  "arthur lira distritao" = "lira",
  "arthur lira bate de frente com o lula" = "lira",
  "arthur lira" = "lira",
  "arthur lira vs pacheco" = "lira",
  "arthur lira stf" = "lira",
  "arthur lira e lula" = "lira",
  "arthur lira preso" = "lira",
  "arthur lira dinheiro" = "lira",
  "arthur lira voto impresso" = "lira",
  "arthur lira vs lula" = "lira",
  
  "bbb 21" = "bbb",
  "racismo bbb" = "bbb",
  "bbb 2021" = "bbb",
  "gil bbb" = "bbb",
  "gilberto bbb21" = "bbb",
  "gil bbb21" = "bbb",
  "gilberto bbb" = "bbb",
  "gil bbb economia" = "bbb",
  "bbb21" = "bbb",
  "fiuk bbb" = "bbb",
  "bbb 20" = "bbb",
  "broxada bbb" = "bbb",
  "bbb 2020" = "bbb",
  "bbb 19" = "bbb",
  "bbb 18" = "bbb",
  "big brother" = "bbb",
  "big brother brasil" = "bbb",
  "big brother brasil 2020" = "bbb",
  
  "cristiano beraldo" = "beraldo",
  "cristiano beraldo jovem pan" = "beraldo",
  "liberaldo" = "beraldo",
  
  "biticoin" = "bitcoin",
  
  "g1" = "globo", 
  
  "bolsomito" = "bolsonaro",
  "bolsonato" = "bolsonaro",
  "bolsonaro 2022" = "bolsonaro",
  "bolsonaro presidente" = "bolsonaro",
  "bolsonaro 2018" = "bolsonaro",
  "jair bolsonaro" = "bolsonaro",
  "bolsonaro2018" = "bolsonaro",
  "bolsonaro corrupto" = "bolsonaro",
  "bolsonaro preso" = "bolsonaro",
  "fora bolsonaro" = "bolsonaro",
  "bolsoanro" = "bolsonaro",
  "bolsonaro genocida" = "bolsonaro",
  "governo bolsonaro" = "bolsonaro",
  "bolsominion" = "bolsominions",
  "bolsonaro e lula" = "lula e bolsonaro",
  "bolsonaro ao vivo" = "bolsonaro",
  "manifestacao bolsonaro" = "bolsonaro",
  
  "guilherme boulos" = "boulos",
  "boulos mst" = "boulos",
  "boulos greve" = "boulos",
  "kim kataguiri vs guilherme boulos" = "kataguiri",
  "kim vs nicolas vs boulos" = "kataguiri",
  "boulos vaza conversa com datena" = "boulos",
  "paola carosella doacao boulos" = "boulos",
  "boulos prefeito" = "boulos",
  
  "buenas ideas" = "buenas ideias",
  
  "camara dos deputados" = "camara",
  "kim kataguiri vence o pt na camara" = "kataguiri",
  "camara acaba com dallagnol" = "deltan",
  "deltan dallagnol camara" = "deltan",
  "camara municipal" = "camara",
  "eduardo bolsonaro na camara" = "eduardo bolsonaro",
  "camara deputados" = "camara",
  "camara dos eua" = "camara",
  "camara de sao paulo" = "camara",
  "kim presidente camara" = "kataguiri",
  "presidencia da camara" = "camara",
  "camara federal" = "camara",
  "suplicy camara" = "suplicy",
  "presidencia camara" = "camara",
  "tv camara" = "camara",
  
  "candidato a prefeitura" = "candidatos",
  "candidatos 2024" = "candidatos",
  "pre candidatura" = "candidatos",
  "candidatos argentina" = "candidatos",
  "candidatura" = "candidatos",
  "candidatura 2024" = "candidatos",
  "candidatura 2026" = "candidatos",
  "guto vai ser o candidato para a prefeitura de sp" = "candidatos",
  "candidatos 2022" = "candidatos",
  "candidatos presidencia 2022" = "candidatos",
  "candidatos governo" = "candidatos",
  "candidatos governo sp" = "candidatos",
  "candidatos presidente 2022" = "candidatos",
  "candidatos a presidencia 2022" = "candidatos",
  "candidatos presidencia" = "candidatos",
  "candidatospresidencia" = "candidatos",
  "candidatos 2018" = "candidatos",
  "candidatos jornal nacional" = "candidatos",
  "candidatos a presidencia" = "candidatos",
  
  "caso carlos" = "carlos bolsonaro",
  "daniela lima e carlos bolsonaro" = "carlos bolsonaro",
  "busca e apreensao carlos bolsonaro" = "carlos bolsonaro",
  "caso carlos bolsonaro" = "carlos bolsonaro",
  "carlos jordy" = "carlos bolsonaro",
  "carlos jordy dia 8" = "carlos jordy",
  "carlos jordy sofre perseguicao politica" = "carlos jordy",
  "carlos jordy esta sendo perseguido" = "carlos jordy",
  "bolsoanro abandonou carla zambelli e deltan dallagnol" = "bolsonaro",
  "carlos jordi" = "carlos jordy",
  "carluxo" = "carlos bolsonaro",
  "mbl carlos bolsonaro" = "carlos bolsonaro",
  "carluxooooo" = "carlos bolsonaro",
  "carlucho" = "carlos bolsonaro",
  
  "canal nostalgia" = "castanhari",
  "castanhari e nyvi" = "castanhari",
  "felipe castanhari" = "castanhari",
  "canal nostalgia amazonia" = "castanhari",
  "nostalgia" = "castanhari",
  
  "censura nao" = "censura",
  "censura" = "censura",
  "anatel vai te censurar" = "censura",
  "lula censura" = "censura",
  "censura pt" = "censura",
  "lula vai te censurar pela anatel" = "censura",
  "pl da censura" = "censura",
  "urgente governo lula vai te censurar" = "censura",
  "censura no brasil" = "censura",
  "censura nas redes sociais" = "censura",
  "censura leo lins" = "censura",
  "censura humorista" = "censura",
  "censura humor" = "censura",
  "censura do bem" = "censura",
  "censura lula" = "censura",
  "censura nao" = "censura",
  "contra censura" = "censura",
  "censura comediante" = "censura",
  "a censura no brasil" = "censura",
  "pl censura" = "censura",
  "leo linins censura" = "censura",
  "leo lins censurado" = "censura",
  "eduardo bolsonaro a favor de censura" = "censura",
  "humorista censurado" = "censura",
  "redes sociais censura" = "censura",
  "redes sociais censuradas" = "censura",
  "lula pl censura2630" = "censura",
  "fomos calados stf vai censurar tudo" = "censura",
  "censura das redes" = "censura",
  "censura midias digitias" = "censura",
  "lula e a censura" = "censura",
  "daniela lima censura" = "censura",
  "a censura comecou fim do telegram" = "censura",
  "censurado" = "censura",
  "censura nas redes" = "censura",
  "censura vai comer solta  o ultimo video do mbl" = "censura",
  "lula e censura" = "censura",
  "lei de censura de piada" = "censura",
  "a censura no humor brasileiro" = "censura",
  "censura livros" = "censura",
  "censura alex jones" = "censura",
  "censura direita" = "censura",
  "censura facebook" = "censura",
  "censura bolsonaro" = "censura",
  "censura redes sociais" = "censura",
  "facebook censura" = "censura",
  "escandalo de censura" = "censura",
  "censura no facebook" = "censura",
  "censura mbl" = "censura",
  "mbl censurado" = "censura",
  
  "ciro gomes" = "ciro",
  "debate ciro gomes" = "ciro",
  "ciro gomes e lula" = "ciro",
  "ciro gomes fala" = "ciro",
  "entrevista ciro gomes" = "ciro",
  "rect ciro gomes" = "ciro",
  "ciro gomes passa pano para o lula" = "ciro",
  "sabatina ciro" = "ciro",
  "ciro no flow" = "ciro",
  "ciro terceira via" = "ciro",
  "ciro 2022" = "ciro",
  "ciro presidente" = "ciro",
  "cirogomes" = "ciro",
  "ciro gomes entrevista" = "ciro",
  "ciro gomes 2022" = "ciro",
  "ciro gomes e sergio moro" = "ciro",
  "ciro fala sobre moro" = "ciro",
  "ciro vs lula" = "ciro",
  "terceira via ciro gomes" = "ciro",
  "ciro gomes desequilibrado" = "ciro",
  "ciro e arthur" = "ciro",
  "ciro e mamaefalei" = "ciro",
  "ciro gomes mente" = "ciro",
  "ciro gomes zueiro" = "ciro",
  "ciro capitao do mato" = "ciro",
  "ciro gomes flagrado" = "ciro",
  "tabata ciro gomes" = "ciro",
  "ciro pdt" = "ciro",
  "ciro ta preso babaca" = "ciro",
  "ciro 12" = "ciro",
  "coronel ciro" = "ciro",
  "ciro mamae falei" = "ciro",
  "ciro gomes e holiday" = "ciro",
  "ciro spc" = "ciro",
  "ciro chama sulistas de nazistas" = "ciro",
  "ciro gomes ataca sul do brasil" = "ciro",
  "ciro12" = "ciro",
  "ciro gomes jornal nacional" = "ciro",
  "ciro jornal nacional" = "ciro",
  "ciro jn" = "ciro",
  "ciro gomes jn" = "ciro",
  "corone ciro" = "ciro",
  "ciro mbl" = "ciro",
  "ciro gomes racismo" = "ciro",
  "ciro fernando holiday" = "ciro",
  "ciro e holiday" = "ciro",
  "ciro holiday" = "ciro",
  
  "cloroquina" = "cloroquina",
  "capita cloroquina" = "cloroquina",
  "capita cloroquina fio cruz" = "cloroquina",
  "eduardo bolsonaro cloroquina" = "cloroquina",
  "bolsonaro defende uso da cloroquina" = "cloroquina",
  "direita cloroquina" = "cloroquina",
  "trump cloroquina" = "cloroquina",
  
  "jornalista cnn" = "cnn",
  "jornalista militante da cnn" = "cnn",
  "cnn esquerda" = "cnn",
  "imagens exclusivas da cnn" = "cnn",
  "cnn brasil" = "cnn",
  "cnn lula" = "cnn",
  "luis miranda cnn" = "cnn",
  "jornalista cnn atacado" = "cnn",
  "liberdade de opiniao cnn" = "cnn",
  "cnn news" = "cnn",
  "entrevista cnn" = "cnn",
  
  "comunistas" = "comunista",
  "comunismo" = "comunista",
  "comunista" = "comunista",
  "confirmado vai rolar debate com comunista da internet" = "comunista",
  "react comunistas" = "comunista",
  "comunistas batendo no mbl" = "comunista",
  "comunistas alugaram um triplex na cabeca do renan" = "comunista",
  "comunistas do youtube" = "comunista",
  "react refutando comunista luideverso" = "comunista",
  "avan comunista" = "comunista",
  "polonia comunista" = "comunista",
  "comunismo na venezuela" = "comunista",
  "professor comunista" = "comunista",
  "proibicao do comunismo" = "comunista",
  "joao comunista" = "comunista",
  "revolucao comunista" = "comunista",
  
  "lula no congresso nacional" = "congresso",
  "nikolas no congresso" = "congresso",
  "flavio dino no congresso" = "congresso",
  "congresso contra lula" = "congresso",
  "congresso" = "congresso",
  "congresso 2022" = "congresso",
  "danilo congresso" = "congresso",
  "congresso 2021" = "congresso",
  "congresso nacional" = "congresso",
  "congresso federal" = "congresso",
  "congresso corrupto" = "congresso",
  "fecha o congresso" = "congresso",
  "treta no congresso" = "congresso",
  "congresso 2018" = "congresso",
  
  "conservador" = "conservadorismo",
  "eneas conservador" = "conservadorismo",
  "conservadores" = "conservadorismo",
  
  "caio coppolla" = "coppolla",
  "caio copolla" = "coppolla",
  "caio coppola" = "coppolla",
  "caio copola" = "coppolla",
  "caio" = "coppolla",
  "caio copola bolsonarista" = "coppolla",
  "caio coppolla humilhou" = "coppolla",
  
  "coronavirus" = "covid",
  "covid 19" = "covid",
  "corona virus" = "covid",
  "covid19" = "covid",
  "cpi da covid" = "covid",
  "cpi covid" = "covid",
  "globo covid" = "covid",
  "bolsonaro positivo covid" = "covid",
  "cura covid" = "covid",
  
  "cmpi" = "cpmi",
  
  "cabo daciolo" = "daciolo",
  
  "deltan dallagnol cassado" = "deltan",
  "cassacao deltan" = "deltan",
  "caso dallagnol" = "deltan",
  "deltan dallagnol" = "deltan",
  "dallagnol" = "deltan",
  
  "dilma rousseff" = "dilma",
  
  "movimento de direita" = "direita",
  "direita politica" = "direita",
  "direita brasileira" = "direita",
  
  "joao doria" = "doria",
  
  "eleicoes 2024" = "eleicoes",
  "eleicoes 2022" = "eleicoes",
  "eleicoes 2018" = "eleicoes",
  "eleicao" = "eleicoes",
  "2022 eleicoes" = "eleicoes",
  "eleicoes2018" = "eleicoes",
  
  "ele nao" = "elenao",
  
  "esquerda brasileira" = "esquerda",
  
  "estados unidos" = "eua",
  
  "falvio dino" = "flavio dino",
  "dino no stf" = "flavio dino",
  
  "flow podcast" = "flow",
  "igor 3k" = "flow",
  "monark e 3k" = "flow",
  "3k" = "flow",
  
  "folha de sao paulo" = "folha",
  
  "formula 1" = "formula1",
  
  "fundo partidario" = "fundao",
  
  "danilo gentile" = "gentili",
  "danilo gentili" = "gentili",
  "gentili presidente" = "gentili",
  
  "gleisi hoffmann" = "gleisi",
  "lula e gleisi" = "gleisi",
  "gleisi e lula" = "gleisi",
  "gleisi hoffmann aeroporto" = "gleisi",
  "jovem nordestino at4cad0 por gleisi hoffmann" = "gleisi",
  "renan santos detona gleisi hoffmann" = "gleisi",
  
  "fernando haddad" = "haddad",
  
  "fernando holiday" = "holiday",
  
  "impeatchment" = "impeachment",
  
  "relacionamento amoroso com jair renan" = "jair renan",
  
  "joven pam" = "jovem pan",
  "jovem pam" = "jovem pan",
  "jovem pan news" = "jovem pan",
  "jovem pan ao vivo" = "jovem pan",
  "jovempan" = "jovem pan",
  
  "kim do mbl" = "kataguiri",
  "movimento do kim" = "kataguiri",
  "kim kataguri" = "kataguiri",
  "kim prefeito" = "kataguiri",
  "kimkataguiri" = "kataguiri",
  "kim deputado" = "kataguiri",
  "cpi do kim" = "kataguiri",
  "debate kim" = "kataguiri",
  "kim kataguiri cpi" = "kataguiri",
  "ascensao do mbl requer kim prefeito" = "kataguiri",
  "deputado kim" = "kataguiri",
  "kim kataguiei" = "kataguiri",
  "kim mst" = "kataguiri",
  "debate kim vs coppolla" = "kataguiri",
  "kim mbl" = "kataguiri",
  "deputado kim kataguiri" = "kataguiri",
  "kim paim panico" = "kataguiri",
  "kim kataguiri vence o pt na camara" = "kataguiri",
  "kim da invertida em ministro de lula" = "kataguiri",
  "kim fala sobre as acoes da rota" = "kataguiri",
  "kim japones" = "kataguiri",
  
  "gay" = "lgbt",
  "lgbtq" = "lgbt",
  "caos por causa da marcha lgbtqia" = "lgbt",
  "lgbtqia" = "lgbt",
  "crianca trans parada lgbt" = "lgbt",
  "parada lgbt" = "lgbt",
  "nos avisamos sobre a agenda tr4ns na parada lgbt" = "lgbt",
  "lgbtfobia" = "lgbt",
  "pastor anti lgbt" = "lgbt",
  "pastor lgbt" = "lgbt",
  "o fim do movimento lgbtqia" = "lgbt",
  "parada lgbt 2023 ao vivo" = "lgbt",
  "uber lgbtqia" = "lgbt",
  "orgulho lgbt" = "lgbt",
  
  "lula hoje" = "lula",
  "luiz inacio lula da silva" = "lula",
  "lula pt" = "lula",
  "lula 2022" = "lula",
  "governo lula" = "lula",
  "lula diz que quer foder o moro" = "lula",
  "declaracao lula" = "lula",
  "fim do lula" = "lula",
  "lula preso" = "lula",
  "indicacao de lula" = "lula",
  "impeachment do lula" = "lula",
  "lula solto" = "lula",
  "lula presidente" = "lula",
  "lula livre" = "lula",
  "lulalivre" = "lula",
  "lula ladrao" = "lula",
  "lula na cadeia" = "lula",
  "lula stf" = "lula", 
  "crimes do lula" = "lula",
  "discurso lula" = "lula",
  
  "bolsonaro e lula" = "lula e bolsonaro",
  "bolsonaro vs lula" = "lula e bolsonaro",
  
  "lula e janja" = "janja", 
  "janja e lula" = "janja",
  "moro lula" = "moro",
  "lula moro" = "moro",
  "lula e moro" = "moro", 
  
  "movimento brasil livre" = "mbl",
  "8 congresso do mbl" = "mbl",
  "canal do mbl" = "mbl",
  "mbl e de direita" = "mbl",
  "mblive" = "mbl",
  "cortes do mbl" = "mbl",
  "mbl videos" = "mbl",
  "mbl e de esquerda" = "mbl",
  "mblivetv" = "mbl",
  "mblive tv" = "mbl",
  "mbl e de ersquerda" = "mbl",
  "lives do mbl" = "mbl",
  "mbl lives" = "mbl",
  "dirceu do mbl" = "mbl",
  "cortes do mblnews" = "mbl",
  "mbl news" = "mbl",
  "podcast do mbl" = "mbl",
  "ficheiro mbl" = "mbl",
  "congresso mbl" = "mbl",
  "academia mbl" = "mbl",
  "documentario mbl" = "mbl",
  "mbl esquerda" = "mbl",
  "mblnews" = "mbl",
  "videos do mbl" = "mbl",
  
  "meteoro brasil" = "meteoro",
  "meteorodoc" = "meteoro",
  
  "ministerio da saude" = "ministro",
  
  "monark talks" = "monark",
  "monark endoidou e vai ter que pagar 4 milhoes" = "monark",
  "caso monark" = "monark",
  "monark tem minhoca na cabeca" = "monark",
  "monark lula" = "monark",
  "monark apoia lula" = "monark",
  "treta nando moura vs monark" = "monark",
  "nando vs monark" = "monark",
  "monark deu fuga no alexandre de moraes" = "monark",
  "monark mbl" = "monark",
  "monark eua" = "monark",
  "monark fujiu para os eua" = "monark",
  "cortes do monarktalks" = "monark",
  "monark ataca o mbl do nada" = "monark",
  "monarktalks" = "monark",
  "monark ataca mbl" = "monark",
  "monark banido" = "monark",
  "nandop manda recdo a monark" = "monark",
  "react nando moura vs monark" = "monark",
  "monark preso" = "monark",
  "monarkl" = "monark",
  "monark pode realmente ser preso por xandao" = "monark",
  "monark e alexandre de moraes" = "monark",
  "cortesmonark" = "monark",
  "discurssao monark" = "monark",
  "entrevista monark" = "monark",
  "monark banido para sempre da internet" = "monark",
  "monark censurado" = "monark",
  "monark debate" = "monark",
  "monark deu fuga no alexandre de moraes" = "monark",
  "monark eua" = "monark",
  "monark fujiu para os eua" = "monark",
  "monark lula" = "monark",
  "monark redes" = "monark",
  "monark tlks" = "monark",
  "nando vs monark" = "monark",
  "react resposta ao monark" = "monark",
  "treta monark" = "monark",
  "treta nando moura vs monark" = "monark",
  
  "sergio moro" = "moro", 
  "juiz sergio moro" = "moro", 
  "sergiomoro" = "moro",
  "moro 2022" = "moro", 
  "propostas do moro" = "moro", 
  "serio moro" = "moro",
  "segio moro" = "moro", 
  "resposta moro" = "moro", 
  "quem mandou matar o moro" = "moro",
  "plano de governo moro" = "moro", 
  "pesquisas moro" = "moro",
  "pacote moro" = "moro",
  "nando moura moro" = "moro",
  "moropresidente" = "moro",
  "moro vai disputar eleicoes" = "moro",
  "moro uniao brasil" = "moro",
  "moro terceira via" = "moro",
  "moro traidor" = "moro",
  "moro suspeito" = "moro",
  "moro stf" = "moro",
  "moro sobre andre mendonca" = "moro",
  "moro preso" = "moro",
  "moro presidente" = "moro",
  "moro podemos" = "moro",
  "moro pesquisa" = "moro",
  "moro o brasil acabou" = "moro",
  "moro no panico" = "moro",
  "moro no flow" = "moro",
  "moro no fantastico" = "moro",
  "moro mensagens whats app" = "moro",
  "moro mbl" = "moro",
  "moro filiacao" = "moro",
  "moro entrevista" = "moro",
  "moro ex ministro da justica" = "moro",
  "quadrilha queira moro" = "moro",
  "moro vai se candidatar" = "moro",
  "moro propostas" = "moro",
  "moro pede demissao" = "moro",
  "moro e mbl" = "moro",
  "moro e arthur do val" = "moro",
  "moro desiste" = "moro",
  "moro defende lava jato" = "moro",
  "moro cia" = "moro",
  "moro candidatura" = "moro",
  "moro candidato a presidencia" = "moro", 
  "moro candidato" = "moro",
  "afinal de contas moro sera preso" = "moro",
  "atentado moro" = "moro",
  "campanha moro" = "moro",
  "cassacao de moro" = "moro",
  "ciro fala sobre moro" = "moro",
  "coppolla fala de moro" = "moro",
  "ex juiz moro" = "moro",
  "filiacao moro" = "moro",
  "governo moro" = "moro",
  "hackers moro" = "moro",
  "intercept moro" = "moro",
  "juis moro" = "moro",
  "janaina paschoal fala sobre moro" = "moro", 
  "juiz moro" = "moro",
  "juiz moro lava jato" = "moro", 
  "lula e bolsonaro querem acabar com moro" = "moro",
  "ministro moro" = "moro",
  
  "cpi mst" = "mst", 
  "cpi do mst" = "mst",
  "acampamento do mst" = "mst", 
  "atos do mst" = "mst",
  "boulos mst" = "mst", 
  "kim mst" = "mst", 
  "lula e mst" = "mst",
  "invasao do mst e expulsa" = "mst",
  "lider do mst" = "mst",
  "invasao do mst" = "mst", 
  "amsterdam" = "mst",
  "lula mst" = "mst", 
  "a milicia do mst" = "mst",
  "como o mst funciona" = "mst", 
  "mbl vs mst" = "mst",
  "mst bahia" = "mst",
  "mst em suzano" = "mst",
  "mst invade terra" = "mst",
  "mst invacoes" = "mst",
  "mst invade" = "mst",
  "mst lula" = "mst",
  "mulher mst" = "mst",
  
  "my news" = "mynews",
  
  "partido novo" = "novo",
  "novo 30" = "novo",
  
  "olavo de carvalho" = "olavo", 
  "mbl ataca olavo" = "olavo",
  "lava toga olavo" = "olavo",
  "mbl x olavo" = "olavo", 
  "morte olavo" = "olavo",
  "olavo briga psl" = "olavo",
  "olavo tem razao" = "olavo",
  "felipe g martins olavo de carvalho" = "olavo",
  "nando moura olavo de carvalho" = "olavo",
  "olavo de carvalho xinga havan" = "olavo",
  
  "pingo nos is" = "pingos nos is",
  "os pingos nos is" = "pingos nos is",
  
  "politica hoje" = "politica",
  "brasil politica" = "politica",
  "acontecimentos politicos" = "politica",
  "cenario politico" = "politica",
  "analise politica" = "politica",
  
  "portas dos fundos" = "porta dos fundos",
  
  "fabricio queiroz" = "queiroz",
  "caso queiroz" = "queiroz",
  "elcio queiroz" = "queiroz", 
  "bolsonaro e o caso queiroz" = "queiroz",
  "flavio queiroz" = "queiroz", 
  "laranja queiroz" = "queiroz", 
  "delegada queiroz" = "queiroz",
  "laranjal queiroz" = "queiroz", 
  "bolsonaro queiroz" = "queiroz",
  "flavio bolsonaro queiroz" = "queiroz", 
  "queiroz atibaia" = "queiroz",
  "queiroz bolsonaro" = "queiroz",
  "queiroz flavio bolsonaro" = "queiroz",
  "queiroz trambiqueiro" = "queiroz",
  "queiroz preso" = "queiroz",
  
  "fim do telegram" = "telegram",
  "acabou o telegram" = "telegram",
  "telegram" = "telegram",
  "telegram banido" = "telegram",
  "xandao derruba telegram" = "telegram",
  
  "donald trump" = "trump",
  "impeachment trump" = "trump",
  "epstein trump" = "trump",
  "caue moura trump" = "trump",
  "discurso trump" = "trump",
  "eleicoes trump" = "trump",
  "bolsonaro e trump" = "trump",
  "react trump discurso" = "trump",
  "trump corona" = "trump",
  "trump e bukele partem para a guerr4" = "trump",
  
  "carla zambeli" = "zambelli",
  "carla zambelli" = "zambelli",
  "carla zambeli briga doria" = "zambelli",
  "carla zambeli policia federal" = "zambelli",
  "carla zambell" = "zambelli",
  "pf carla zambelli" = "zambelli",
  "carla zambelli fake news" = "zambelli",
  "carla zambelli bolsonaro fake news" = "zambelli",
  "carla zambelli jean willys" = "zambelli",
  "carla zambelli havan" = "zambelli",
  "react carla zambelli chamada de burra pelo eduardo" = "zambelli",
  
  "alexandre de moraes vs elon musk" = "musk",
  "elon musk vs alexandre de moraes" = "musk",
  "bolsonaro e eleon musk" = "musk",
  "elon musk e alexandre" = "musk",
  "lula fala de elon musk" = "musk",
  "elon musk anuncia candidatura " = "musk",
  "elon musk humilha lula" = "musk",
  "lula fala do foguete de elon musk" = "musk",
  "elon musk vs mark zuckerberg" = "musk",
  "elon musk vs xandao" = "musk",
  "lula fala mal de elon musk" = "musk",
  "elon musk vs lula" = "musk",
  "elon musk esta com o capitao" = "musk",
  "elon musk brasil" = "musk",
  "lula vs elon musk" = "musk",
  "manifestacao elon musk" = "musk",
  "musk" = "musk",
  
  "a missao" = "missao",
  "5 coisas que voce nao sabia sobre a missao" = "missao",
  "a missao esta lancada" = "missao",
  "mbl missao" = "missao",
  "missao" = "missao",
  "partido missao" = "missao",
  
  "contra pt" = "pt",
  "deputados contra o pt" = "pt",
  "governo do pt" = "pt",
  "lula pt" = "pt",
  "corrupcao pt" = "pt",
  "critica pt" = "pt",
  "crimes do pt" = "pt",
  "a corrupcao do pt" = "pt",
  "bndes pt" = "pt",
  "governo pt" = "pt",
  "debate mbl vs pt" = "pt",
  "como faremos para acabar com o pt" = "pt",
  "crypto moedas" = "crypto",
  "governto pt" = "pt",
  "como fazer para lutar contra o pt e o lula" = "pt",
  "cpi do dia 8 ja esta dando ruim pro pt" = "08/01",
  "lula corrupto" = "lula",
  "cid gomes pt" = "pt",
  "partido dos trabalhadores" = "pt",
  "petista" = "pt"
)

# segundo vetor de substituições
substituicoes_2 <- c("kimkataguiri" = "kataguiri",
                     "mbltv" = "mbl",
                     "eleicoes2022" = "eleicoes",
                     "bolsonarolive" = "bolsonaro",
                     "bolsonaro17" = "bolsonaro",
                     "elon musk" = "musk",
                     "cpido08/01" = "08/01",
                     "conservadorismoismo" = "conservadorismo",
                     "bolsominionss" = "bolsominion"
)

# terceiro vetor de substituições
substituicoes_3 <- c("elonmusk" = "musk",
                     "b17" = "bolsonaro",
                     "manifestacoes" = "manifestacao",
                     "bolsonaristas" = "bolsonarista",
                     "deputados"  = "deputado",
                     "descealetra" = "cauemoura",
                     "cauemora" = "cauemoura",
                     "cauevsarthurdoval" = "arthurdoval",
                     "arthurdovalvccaue" = "arthurdoval",
                     "cauevsmoura" = "cauemoura",
                     "cauemouraarregao" = "cauemoura",
                     "indicacaoparaostf"  = "stf",
                     "indicacaostf" = "stf",
                     "indios" = "indigena",
                     "india" = "indigena", 
                     "indiajanja" = "janja",
                     "janjaindia" = "janja",  
                     "janjanaindia" = "janja",                       
                     "janjadancaindia" = "janja",           
                     "lulaindicadino" = "dino",        
                     "lulanaindia" = "lula",
                     "indio" = "indigena",
                     "terrasindigenas" = "indigena",
                     "^ancap" = "anarcocapitalismo"
                    
                     )

# vetor de substituições sobre anarcocapitalismo
substituicoes_ancap <- c("podcastmercadofinanceiro" = "mercadofinanceiro",
                         "invasaodepropriedades" = "propriedade",
                         "imprensalivre" = "livreimprensa",
                         "segurancapublicanobrasil" = "segurancapublica",
                         "ministroseguranca" = "seguranca",
                         "segurancaoublicadino" = "segurancapublica",
                         "gabinetedesegurancainstitucional" = "seguranca",
                         "policiadainternet" = "censura",
                         "liberallismo" = "liberal",
                         "javiermilei" = "milei",
                         "javiermileimejoresmomentos" = "milei",
                         "mileivenceuaseleicoesprimarias" = "milei",
                         "milei" = "milei",
                         "mileielouco" = "milei",
                         "quemejaviermilei" = "milei",
                         "criancatrans" = "agendatrans",
                         "cuidadoseufilhoseratrans" = "agendatrans",
                         "criancamortaportrans" = "agendatrans",
                         "lulaeagendatrans" = "agendatrans",
                         "materiaglobotrans" = "agendatrans",
                         "lgbtcriancatrans" = "agendatrans",
                         "estupradortrans" = "agendatrans",
                         "misstrans" = "agendatrans",
                         "nadadoratrans" = "agendatrans",
                         "transexpulso" = "agendatrans",
                         "transfobia" = "agendatrans",
                         "transassassina" = "agendatrans",
                         "transeexpulsodasolimpiadas" = "agendatrans"
)
                        

### Visualizar grupos de susbstituições ========================================
agrupamentos <- split(names(substituicoes), substituicoes)
agrupamentos

agrupamentos2 <- split(names(substituicoes_2), substituicoes_2)
agrupamentos2

agrupamentos3 <- split(names(substituicoes_3), substituicoes_3)
agrupamentos3

agrupamentos4 <- split(names(substituicoes_ancap), substituicoes_ancap)
agrupamentos4

### Substituindo ===============================================================
tags_mod2 <- tags_mod2 %>%
  mutate(Source = str_replace_all(Source, substituicoes)) %>% 
  mutate(Source = str_replace_all(Source, " ", ""))

tags_mod2 <- tags_mod2 %>%
  mutate(Source = str_replace_all(Source, substituicoes_2))

tags_mod2 <- tags_mod2 %>%
  mutate(Source = str_replace_all(Source, substituicoes_3))

tags_mod2 <- tags_mod2 %>%
  mutate(Source = str_replace_all(Source, substituicoes_ancap))

### Filtragem e recorte das mais frequentes ====================================
# Definir o valor mínimo de frequência
min_freq <- 50

# Calcular top 10% de tags mais frequentes
top_10_percent <- ceiling(nrow(tags_mod2) * 0.1)

# Calcular frequência das tags, filtrar e selecionar as top 10% mais frequentes
### Pessoas ====================================================================
tags_pessoas <- tags_mod2 %>% 
  count(Source, name = "freq") %>%
  arrange(desc(freq)) %>%
  filter(freq >= min_freq) %>%
  top_n(top_10_percent, wt = freq) %>% 
  filter(!(Source %in% c("mbl", "brasil", "direita", "livre", "movimento", 
                         "eleicoes", "censura", "politica", "pt", "jovempan", 
                         "esquerda", "comunista", "cnn", "candidatos", "flow", 
                         "mst", "covid", "congresso", "cpi", "bbb", "corrupcao", 
                         "camara", "globo", "imposto", "3em1", "bolsonarismo", 
                         "lavajato", "buenasideias", "eua", "impeachment", "liberalismo", 
                         "meteoro", "lgbt", "folha", "fakenews", "08/01", 
                         "economia", "elenao", "conservadorismo","manifestacao", 
                         "governo", "mynews", "cpmi", "centrao", "kim",
                         "podcast", "capitalismo", "manueladavila", "debate",
                         "ideiasradicais", "pingosnosis", "bolsopetismo", 
                         "amazonia", "feminismo", "catracalivre", "china", 
                         "portadosfundos", "brasilia", "ilhadebarbados", "nazismo",
                         "guerra", "ministro", "brasilparalelo", "briga", "ditadura",
                         "2022", "crise", "educacao", "novo", "psol", "bandido", 
                         "lacracao", "carnaval", "golpe", "alesp", "psl", "agronegocio",
                         "gasolina", "bitcoin","greve", "futebol", "ideologiadegenero",
                         "anarcocapitalismo", "choquei", "imprensa", "panico",
                         "cadeia","bndes", "argentina", "bolsonarista", "mito", 
                         "estadao", "franca", "armas", "deputado", "caminhoneiros",
                         "cuba", "antagonista", "cancelamento", "israel", "agendatrans",
                         "facebook", "rafinhabastos", "cloroquina", "missao", "acordao",
                         "datafolha", "libertario", "presidente", "analisecomportamental",
                         "bolsominions", "telegram", "campanha", "comandovermelho",
                         "inimigospublicos", "justica", "pl2630", "crimeorganizado",
                         "fundao", "democraciaemvertigem", "stf", "amazon", "ceara",
                         "coaf", "copa", "lacradores", "previdencia", "bondedorole",
                         "agendawoke", "ai5", "bahia", "atentado", "aliexpress",
                         "mtst", "cultura", "elesim", "fantastico", "favela", "aborto",
                         "dem", "desarmamento", "17", "abordagempolicial", "caixa2",
                         "central", "cota", "formula1", "historia", "bolsadevalores",
                         "comediante", "crime", "marvel", "guaruja", "copadomundo",
                         "casorota", "spotniks", "aderiva", "netflix", "07/09", 
                         "documentario", "feminista", "brexit", "cassado", "polemica",
                         "psdb", "entrevista", "batman", "chile", "coronavac"
                    )))

###### Wordcloud ===============================================================
wc_pessoas <- wordcloud2(data = tags_pessoas,
                 color = "random-dark",
                 size = 1,
                 minSize = 5,
                 gridSize = 10,
                 minRotation = 0, 
                 maxRotation = 0, 
                 rotateRatio = 1)

saveWidget(wc_pessoas,"wc_pessoas.html", selfcontained = F)

webshot("wc_pessoas.html","wc_pessoas.png", 
        delay = 5, 
        vwidth = 1000, 
        vheight= 1000)

###### Gráfico =================================================================
grafico_pessoas <- tags_pessoas %>%
  top_n(20, wt = freq) %>% 
  ggplot(aes(reorder(Source, freq), freq, fill = Source)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "", 
       title = "20 pessoas mais frequentes") + 
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0))

### Temas ======================================================================
tags_temas <- tags_mod2 %>% 
  count(Source, name = "freq") %>%
  arrange(desc(freq)) %>%
  filter(freq >= min_freq) %>%
  top_n(top_10_percent, wt = freq) %>% 
  filter((Source %in% c("direita", "eleicoes", "censura", "politica", "pt", "jovempan", 
                         "esquerda", "comunista", "cnn", "candidatos", "flow", 
                         "mst", "covid", "congresso", "cpi", "bbb", "corrupcao", 
                         "camara", "globo", "imposto", "3em1", "bolsonarismo", 
                         "lavajato", "buenasideias", "eua", "impeachment", "liberalismo", 
                         "meteoro", "lgbt", "folha", "fakenews", "08/01", 
                         "economia", "elenao", "conservadorismo","manifestacao", 
                         "governo", "mynews", "cpmi", "centrao", 
                         "podcast", "capitalismo", "debate",
                         "ideiasradicais", "pingosnosis", "bolsopetismo", 
                         "amazonia", "feminismo", "catracalivre", "china", 
                         "portadosfundos", "brasilia", "ilhadebarbados", "nazismo",
                         "guerra", "ministro", "brasilparalelo", "briga", "ditadura",
                         "2022", "crise", "educacao", "novo", "psol", "bandido", 
                         "lacracao", "carnaval", "golpe", "alesp", "psl", "agronegocio",
                         "gasolina", "bitcoin","greve", "futebol", "ideologiadegenero",
                         "anarcocapitalismo", "choquei", "imprensa", "panico",
                         "cadeia","bndes", "argentina", "bolsonarista", "mito", 
                         "estadao", "franca", "armas", "deputado", "caminhoneiros",
                         "cuba", "antagonista", "cancelamento", "israel", "agendatrans",
                         "facebook", "rafinhabastos", "cloroquina", "missao", "acordao",
                         "datafolha", "libertario", "presidente", "analisecomportamental",
                         "bolsominions", "telegram", "campanha", "comandovermelho",
                         "inimigospublicos", "justica", "pl2630", "crimeorganizado",
                         "fundao", "democraciaemvertigem", "stf", "amazon", "ceara",
                         "coaf", "copa", "lacradores", "previdencia", "bondedorole",
                         "agendawoke", "ai5", "bahia", "atentado", "aliexpress",
                         "mtst", "cultura", "elesim", "fantastico", "favela", "aborto",
                         "dem", "desarmamento", "17", "abordagempolicial", "caixa2",
                         "central", "cota", "formula1", "historia", "bolsadevalores",
                         "comediante", "crime", "marvel", "guaruja", "copadomundo",
                         "casorota", "spotniks", "aderiva", "netflix", "07/09", 
                         "documentario", "feminista", "brexit", "cassado", "polemica",
                         "psdb", "entrevista", "batman", "chile", "coronavac"
                         
  )))

###### Wordcloud ===============================================================
wc_temas <- wordcloud2(data = tags_temas,
                         color = "random-dark",
                         size = 1,
                         minSize = 7,
                         gridSize = 15,
                         minRotation = 0, 
                         maxRotation = 0, 
                         rotateRatio = 1)

saveWidget(wc_temas,"wc_temas.html", selfcontained = F)

webshot("wc_temas.html","wc_temas.png", 
        delay = 5, 
        vwidth = 800, 
        vheight= 800)

###### Gráfico =================================================================
grafico_temas <- tags_temas %>%
  top_n(20, wt = freq) %>% 
  ggplot(aes(reorder(Source, freq), freq, fill = Source)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "", 
       title = "20 temas mais frequentes") + 
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0))

### Ancap ======================================================================
# separando arquivo de nós para serem tratados no Gephi
tags_mod3 <- tags_mod %>%
  mutate(
    Source = str_replace_all(Source, substituicoes) %>%
      str_replace_all(" ", "") %>%
      str_replace_all(substituicoes_2) %>%
      str_replace_all(substituicoes_3) %>%
      str_replace_all(substituicoes_ancap),
    Target = str_replace_all(Target, substituicoes) %>%
      str_replace_all(" ", "") %>%
      str_replace_all(substituicoes_2) %>%
      str_replace_all(substituicoes_3) %>%
      str_replace_all(substituicoes_ancap)
  ) %>%
  filter((Source %in% c("liberalismo", "08/01", "economia", "imposto", "lgbt",
                        "fakenews", "musk", "capitalismo", "ideiasradicais",
                        "conservadorismo", "brasilparalelo", "nazismo", "guerra",
                        "lacracao", "golpe", "bitcoin", "agronegocio", "taxacao",
                        "ideologiadegenero", "anarcocapitalismo", "armas", 
                        "missao", "libertario", "lacradores", "previdencia", 
                        "agendawoke", "ai5", "cultura", "aborto", "desarmamento", 
                        "crime", "corrupcao", "crypto", "assassinato", "fascismo", 
                        "globolixo", "artigo142", "doutrinacao","livrecomercio", 
                        "livreimprensa", "mercado", "livremercado", "reformaliberal",
                        "censura", "seguranca", "segurancapublica", "liberdade", 
                        "institutoliberal","liberais", "liberal",
                        "liberdadeeconomica", "movimentoliberal", "rothbard", "hayek",
                        "agendatrans"
                        )))
          
write.csv(tags_mod3, "nos_ancap.csv", row.names = FALSE )