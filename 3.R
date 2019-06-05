setwd("~/Documentos/Datos complejos/Tarea3")
library(wordVectors)
library(magrittr)


## Preprocesando los archivos para generar las inmersiones en Word2Vec
#El codigo esta comentado debido a que ya estan generados los bin

#if (!file.exists("../data/spanish_prep.txt")) prep_word2vec(origin = "../data/es-en/europarl-v7.es-en.es", destination = "../data/spanish_prep.txt",
#                lowercase = T, bundle_ngrams = 2)

#if (!file.exists("../data/english_prep.txt")) prep_word2vec(origin = "../data/es-en/europarl-v7.es-en.en", destination = "../data/english_prep.txt",
#                lowercase = T, bundle_ngrams = 2)


## Entrenando los archivos binarios

# Generamos 150 vectores para reducir un poco el tiempo de computo

# if (!file.exists("../data/euro_spanish_vecs.bin")) {
#   model = train_word2vec("/spanish_prep.txt",
#                          "/euro_spanish_vecs.bin",
#                          vectors = 150, threads = 6,
#                          window = 12, iter = 5,
#                          negative_samples = 0, force=TRUE)
model = read.vectors("euro_spanish_vecs.bin")   #Leemos el modelo de las palabras en espa;ol

#  model = train_word2vec("/english_prep.txt",
                         #"/euro_english_vecs.bin",
                         # vectors = 150, threads = 8,
                         # window = 12, iter = 5,
                         # negative_samples = 0, force=TRUE)
model.en = read.vectors("euro_english_vecs.bin") #Leemos el modelo de las palabras en ingles


# Google Translate

library(googleLanguageR)

gl_auth("CIMAT1 2018-92638ee9d78g.json")

library(data.table)
library(text2vec)

## Translate 5K terms from source language

path.en <- "english_prep.txt"
path.es <- "spanish_prep.txt"

raw.en <- readChar(path.en, file.info(path.en)$size)  # Train ingles
raw.es <- readChar(path.es, file.info(path.es)$size)  # Train español

it = itoken(raw.es, tolower, word_tokenizer, n_chunks = 10)  # Corpus en español !!
vocab = create_vocabulary(it)
pruned_vocab = prune_vocabulary(vocab, term_count_min = 10)  # Bolsa de palabras

n <- nrow(pruned_vocab)
nobs <- 5 * 1000
fiveK.terms <- pruned_vocab[n - (0:(nobs-1)), c(1, 2)]  # Extrayendo 5 mil terminos mas comunes

# Traduciendo los 5 mil terminos mas comunes

es_en <- gl_translate(fiveK.terms$term, target = "en")

path.trans <- "translation_5K_es_en.csv"
if(!file.exists(path.file.trans))
  write.csv(es_en, file = path.trans, row.names = F)   #Colocamos las palabras traducidas en un csv

# Compute W translation matrix

path.trans <- "translation_5K_es_en.csv"

# If you already have the translation just modify the next file path
es_en <- read.csv(path.trans)  #leemos el csv

## Generamos un match con las palabras en español

#Colocamos los terminos en la fuente y el objetivo
target <- attr(model.en@.Data, "dimnames")[[1]]
source <- attr(model@.Data, "dimnames")[[1]]

# Hacemos un join para filtrar de la matriz de word embbedings con las palabras en nuestro corpus
ans.match <- match(es_en$translatedText, target)
src.match <- match(es_en$text, source)

#Generamos los vectores z y x
Z <- model.en@.Data[ans.match[joined.idx], ]  # target language

X <- model@.Data[src.match[joined.idx], ]  # source language

X <- as.data.frame(X)  # we need to cast as DF (feature matrix)
colnames(Z) <- paste0("R", 1:ncol(Z))

## Generamos la matriz de traduccion W
W <- lm(Z ~ ., data = X)  

## Generamos unos ejemplos para probar la traduccion

nearest1 <- model %>% closest_to("derecho")
example1 <- model[[nearest1$word[1]]]@.Data
example1.pred <- predict(W, newdata = as.data.frame(example1))
model.en %>% closest_to(example1.pred)  # Traduccion

nearest2 <- model %>% closest_to("derechos_humanos")
example2 <- model[[nearest2$word[1]]]@.Data
example2.pred <- predict(W, newdata = as.data.frame(example2))
model.en %>% closest_to(example2.pred)  # Traduccion

nearest3 <- model %>% closest_to("medio_ambiente")
example3 <- model[[nearest3$word[1]]]@.Data
example3.pred <- predict(W, newdata = as.data.frame(example3))
model.en %>% closest_to(example3.pred)  # Traduccion

nearest4 <- model %>% closest_to("borracho")  #Ejemplo de traduccion inexacta
example4 <- model[[nearest4$word[1]]]@.Data
example4.pred <- predict(W, newdata = as.data.frame(example4))
model.en %>% closest_to(example4.pred)  # Traduccion

nearest5 <- model %>% closest_to("compita")  #Ejemplo de traduccion inexacta
example5 <- model[[nearest5$word[1]]]@.Data
example5.pred <- predict(W, newdata = as.data.frame(example5))
model.en %>% closest_to(example5.pred)  # Traduccion
