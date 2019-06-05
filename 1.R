############Ejercicio 1
###################################### Incisco a ############################################################################
rm(list=ls())
library(tm)
mat.terms <- function(rut.data){
  corp <- Corpus(DirSource(rut.data,recursive=TRUE,encoding = "ISO-8859-1"),readerControl = list(reader = readPlain, language = "es", load = TRUE))
  ##inspect(aa[[1]]) ## por si quieres visualizar el texto de un documento
  ## preproceso
  corp <- tm_map(corp,stripWhitespace)
  corp <- tm_map(corp,removeNumbers)
  corp <- tm_map(corp,content_transformer(tolower))
  corp <- tm_map(corp,removePunctuation)
  corp <- tm_map(corp,removeWords,stopwords("spanish"))
  corp<- tm_map(corp,content_transformer(gsub),       #Removemos simbolos y palabras que no aportan valor al analisis
         pattern="([(a-zA-Z0-9)])\\.([a-zA-Z0-9])",
         replacement="\\1. \\2")
  corp<- tm_map(corp,content_transformer(gsub),
         pattern="([(a-zA-Z0-9)])\\.([a-zA-Z0-9])",
         replacement="\\1, \\2")
  corp<-tm_map(corp,content_transformer(gsub),
         pattern="\\S+@\\S+",replacement="")
  ## obtiene matriz de terminos
  tdm <- TermDocumentMatrix(corp,control=list(minDocFreq=10))
  ## remuevo terminos poco repetidos (95% sparsity)
  tdm <- removeSparseTerms(tdm, 0.95)
  ## frecuencia de terminos
  term.freq <- rowSums(as.matrix(tdm))
  sort.freq <- sort.int(term.freq,decreasing=TRUE,index.return=FALSE)
  return(list(term.freq.sort=sort.freq,
              tdm=tdm,
              terms=names(sort.freq),
              nterms=length(terms),
              ndocs=length(corp)))
}

## conteo de terminos relacionados con alguna categoria
## se usa posteriormente para obtener verosimilitudes
count.terms <- function(tdms,terms,laplace.smooth=TRUE){
  freq.table <- unlist(lapply(terms,function(x)sum(tm_term_score(tdms,x))))
  if(laplace.smooth)
    freq.table <- freq.table+1
  freq.table <- data.frame(freq.table)
  rownames(freq.table) <- terms
  return(freq.table)
}

## clasificacion con Naive-Bayes
## para este ejemplo, 1 es negativo (N), 2 es positivo (P)
test.bayes <- function(tdm.test,loglik1,loglik2,logp1,logp2,terms.v){
  ## para todos los documentos de prueba
  c <- rep(0,ncol(tdm.test))
  for(i in 1:ncol(tdm.test)){
    ## buscar logverosimilitudes de terminos en el vocabulario
    t.i <- rownames(tdm.test)[tdm.test[,i]>0]
    aa <- lapply(t.i,function(x) which(x==rownames(loglik1)))
    sum.loglik1 <- sum(loglik1[unlist(aa),])
    aa <- lapply(t.i,function(x) which(x==rownames(loglik2)))
    sum.loglik2 <- sum(loglik2[unlist(aa),])
    p1 <- logp1+sum.loglik1
    p2 <- logp2+sum.loglik2
    temp <- c(p1,p2)
    c[i] <- which(temp==max(temp))
  }
  temp <- c("N","P")
  return(temp[c])
}


## obtengo las matrices de terminos en los documentos de ambas categorias
rut.data <- "~/Documentos/Datos complejos/Tarea3/reviews/SFU_spanish_reviews_train_test/train/no"
comp.neg <- mat.terms(rut.data)
rut.data <- "~/Documentos/Datos complejos/Tarea3/reviews/SFU_spanish_reviews_train_test/train/yes"
comp.pos <- mat.terms(rut.data)
## matriz de terminos para todos los documentos
rut.data <- "~/Documentos/Datos complejos/Tarea3/reviews/SFU_spanish_reviews_train_test/train/"
comp.all <- mat.terms(rut.data)
## verosimilitudes
count1 <- count.terms(comp.neg$tdm,comp.all$terms)
loglik1 <- log(count1/sum(count1))
count2 <- count.terms(comp.pos$tdm,comp.all$terms)
loglik2 <- log(count2/sum(count2))
## aprioris
logp1 <- log(comp.neg$ndocs/comp.all$ndocs)
logp2 <- log(comp.pos$ndocs/comp.all$ndocs)
## error entrenamiento
tdm.train <- as.matrix(comp.all$tdm)
clas.est.train <- test.bayes(tdm.train,loglik1,loglik2,logp1,logp2,comp.all$terms)
clas.orig.train<-c(rep("N",160),rep("P",160))
table(clas.orig.train,clas.est.train)  #Tabla de confusion de la prediccion del entrenamiento
round(sum(diag(table(clas.orig.train,clas.est.train)))/sum(table(clas.orig.train,clas.est.train))*100,2) #Nivel de prediccion para datos de entrenamiento

#Prediciendo los datos de prueba
rut.data <- "~/Documentos/Datos complejos/Tarea3/reviews/SFU_spanish_reviews_train_test/test/"
comp.all.test <- mat.terms(rut.data)
tdm.test <- as.matrix(comp.all.test$tdm)
clas.est.test <- test.bayes(tdm.test,loglik1,loglik2,logp1,logp2,comp.all$terms)
clas.orig.test<-c(rep("N",40),rep("P",40))
table(clas.orig.test,clas.est.test)
round(sum(diag(table(clas.orig.test,clas.est.test)))/sum(table(clas.orig.test,clas.est.test))*100,2) #Nivel de prediccion para datos de prueba

###################################### Incisco b ############################################################################
#Modificamos la funcion original para tener los pesos mediante TF-IDF
mat.terms.Itfidf <- function(rut.data){
  corp <- Corpus(DirSource(rut.data,recursive=TRUE,encoding = "ISO-8859-1"),readerControl = list(reader = readPlain, language = "es", load = TRUE))
  ##inspect(aa[[1]]) ## por si quieres visualizar el texto de un documento
  ## preproceso
  corp <- tm_map(corp,stripWhitespace)
  corp <- tm_map(corp,removeNumbers)
  corp <- tm_map(corp,content_transformer(tolower))
  corp <- tm_map(corp,removePunctuation)
  corp <- tm_map(corp,removeWords,stopwords("spanish"))
  corp<- tm_map(corp,content_transformer(gsub),       #Removemos simbolos y palabras que no aportan valor al analisis
                pattern="([(a-zA-Z0-9)])\\.([a-zA-Z0-9])",
                replacement="\\1. \\2")
  corp<- tm_map(corp,content_transformer(gsub),
                pattern="([(a-zA-Z0-9)])\\.([a-zA-Z0-9])",
                replacement="\\1, \\2")
  corp<-tm_map(corp,content_transformer(gsub),
               pattern="\\S+@\\S+",replacement="")
  ## obtiene matriz de terminos
  tdm <- TermDocumentMatrix(corp,control=list(minDocFreq=10,weighting = function(x) weightTfIdf(x, normalize = FALSE),
                                              stopwords = TRUE))  #Aqui modificamos para obtener los pesos TF-IDF
  ## remuevo terminos poco repetidos (95% sparsity)
  tdm <- removeSparseTerms(tdm, 0.95)
  ## frecuencia de terminos
  term.freq <- rowSums(as.matrix(tdm))
  sort.freq <- sort.int(term.freq,decreasing=TRUE,index.return=FALSE)
  return(list(term.freq.sort=sort.freq,
              tdm=tdm,
              terms=names(sort.freq),
              nterms=length(terms),
              ndocs=length(corp)))
}
#Leeemos todos los documentos para hacer la matriz de pesos TF-IDF
rut.data <- "~/Documentos/Datos complejos/Tarea3/reviews/SFU_spanish_reviews_train_test/"
comp.all.tf <- mat.terms.Itfidf(rut.data)
count<-t(as.matrix(comp.all.tf$tdm)) #Trasponemos para que como filas tengamos los documentos
etiquetas<-c(rep(-1,40),rep(1,40), rep(-1,160), rep(1,160)) #Etiquetamos -1 para negativo y 1 para positivo
count<-as.data.frame(cbind(etiquetas,count)) #generamos la matriz con etiquetas

#Clasificamos usando SVM y la matriz de pesos TF-IDF
library(e1071)
model <- svm(etiquetas ~ ., data = count[81:length(etiquetas),], kernel="sigmoid") #Entrenamos solo con los documentos de entrenamiento
pred <- sign(predict(model, count[1:80,-1])) #Predecimos con el SVM entrenado
table(etiquetas[1:80],pred) #Generamos una tabla de confusion
round(sum(diag(table(etiquetas[1:80],pred)))/sum(table(etiquetas[1:80],pred))*100,2) #Nivel de prediccion para datos de prueba

#Clasificamos usando CART y la matriz de pesos TF-IDF
library(rpart)
set.seed(100) #Fijamos una semilla para que se puedan reproudcir los resultados
model.tree <- rpart(etiquetas ~ ., data = count[81:length(etiquetas),], 
                    method="class",control = rpart.control(cp = 0.05, maxdepth = 30), xval=10) #Entrenamos un modelo de tipo CART
pred2 <- predict(model.tree, count[1:80,-1],type = "class") #Predecimos con el modelo CART entrenado
table(etiquetas[1:80],pred2)
round(sum(diag(table(etiquetas[1:80],pred2)))/sum(table(etiquetas[1:80],pred2))*100,2) #Nivel de prediccion para datos de prueba

#Clasificamos usando CART y la matriz de pesos TF-IDF
library(nnet)
model.nnet <- nnet(etiquetas ~ ., data = count[81:length(etiquetas),], size=1,linout=T)
pred3 <- sign(predict(model.nnet, count[1:80,-1])) #Predecimos con el modelo nnet entrenado
table(etiquetas[1:80],pred3)
round(sum(diag(table(etiquetas[1:80],pred3)))/sum(table(etiquetas[1:80],pred3))*100,2) #Nivel de prediccion para datos de prueba

###################################### Incisco c ############################################################################

library(wordVectors)
library(magrittr)
library(tsne)
#if (!file.exists("reviews.txt")) prep_word2vec(origin="~/Documentos/Datos complejos/Tarea3/reviews/SFU_spanish_reviews_train_test/train",destination="reviews.txt",lowercase=T,bundle_ngrams=2)
#model = train_word2vec("~/Documentos/Datos complejos/Tarea3/reviews.txt",output="~/Documentos/Datos complejos/Tarea3/reviews_vectors.bin",threads = 3,vectors = 200,window=12,force=TRUE)
model = read.vectors("reviews_vectors.bin")

#Observando algunas cercanias entre las palabras usando word2vec y su representación en los primeros 2 componentes principales
near2<-closest_to(model,model[[c("bueno","bien","confianza","buena")]],20)
near2.a = model[[near2$word,average=F]]
plot(near2.a,method="pca")
title(main="Palabras: bueno, bien, confianza, buena")

near1<-closest_to(model,model[[c("pelicula")]],10)
near1.c = model[[near1$word,average=F]]
plot(near1.c,method="pca")
title(main="Palabras: pelicula")

near1<-closest_to(model,model[[c("computadora")]],10)
near1.c = model[[near1$word,average=F]]
plot(near1.c,method="pca")
title(main="Palabras: computadora")

near1<-closest_to(model,model[[c("lavadora")]],10)
near1.c = model[[near1$word,average=F]]
plot(near1.c,method="pca")
title(main="Palabras: lavadora")

near1<-closest_to(model,model[[c("coche")]],10)
near1.c = model[[near1$word,average=F]]
plot(near1.c,method="pca")
title(main="Palabras: coche")



