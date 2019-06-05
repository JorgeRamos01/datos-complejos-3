setwd("~/Documentos/Datos complejos/Tarea3")
library(wordVectors)
library(magrittr)
library(tsne)

#Para entrenar se pueden usar los comandos que se encuentran comentados a continuacion

#if (!file.exists("billion.txt")) prep_word2vec(origin="~/Documentos/Datos complejos/Tarea3/billion/",destination="billion.txt",lowercase=T,bundle_ngrams=2)
#model1 = train_word2vec("~/Documentos/Datos complejos/Tarea3/billion.txt",output="~/Documentos/Datos complejos/Tarea3/billion_vectors.bin",threads = 5,vectors = 200,window=12,force=TRUE)

#una vez entrenado el word2vectors se puede usar el .bin
model1 =read.vectors("billion_vectors.bin")

#Observando algunas cercanias entre las palabras usando word2vec
near.billion<-nearest_to(model1,model1[[c("laboral", "trabajo", "empresa")]],10)
plot(near.billion,type="n", main="Palabras cercanas a laboral, trabajo y empresa", ylab="Peso", xlab="Indice", sub="Word2Vec")
text(x=seq(1,10,1), y=near.billion,names(near.billion), col=c("red","red","red", rep("black",7)))

near.billion2<-nearest_to(model1,model1[[c("desarrollo", "desarrollo_económico")]],10)
plot(near.billion2,type="n", main="Palabras cercanas a desarrollo y desarrollo económico", ylab="Peso", xlab="Indice", sub="Word2Vec")
text(x=seq(1,10,1), y=near.billion2,names(near.billion2), col=c("red", "red",rep("black",8)))

near.billion3<-nearest_to(model1,model1[[c("política", "confrontación")]],10)
plot(near.billion3,type="n", main="Palabras cercanas a política y confrontación", ylab="Peso", xlab="Indice", sub="Word2Vec")
text(x=seq(1,10,1), y=jitter(near.billion3),names(near.billion3), col=c("red","red", rep("black",8)))

#Un grafico en las primeras 2 componentes principales de las palabras en los documentos de entrenamiento

plot(model1) #Representacion en 2 dimensiones de las palabras en el conjunto de entrenamiento

centers = 10   #Generamos 10 clusters usando kmeans para encontrar algunas asociaciones posibles
clustering1 = kmeans(model1,centers=centers,iter.max = 40)
#Mostramos algunas de las palabaras acomodadas por cluster
names(clustering1$cluster[clustering1$cluster==3][1:20])
names(clustering1$cluster[clustering1$cluster==4][1:20])
names(clustering1$cluster[clustering1$cluster==5][1:20])
names(clustering1$cluster[clustering1$cluster==8][1:20])


centers2 = 5   #Generamos 5 clusters usando kmeans para encontrar algunas asociaciones posibles
clustering2 = kmeans(model1,centers=centers2,iter.max = 40)
#Mostramos algunas de las palabaras acomodadas por cluster
names(clustering2$cluster[clustering2$cluster==1][1:20])
names(clustering2$cluster[clustering2$cluster==2][1:20])
names(clustering2$cluster[clustering2$cluster==3][1:20])
names(clustering2$cluster[clustering2$cluster==4][1:20])
names(clustering2$cluster[clustering2$cluster==5][1:20])


