base = read.csv ("ortopedic_3C.csv")

summary (base)

#rename column
names(base)[names(base)=="class"]<-"classe" 

base$classe = factor(base$classe, levels = c('Normal', 'Hernia', 'Spondylolisthesis'), labels = c(0,1,2))

# ML - KNN
base[, 1:6] = scale(base[, 1:6])

library(caTools)
set.seed(1)
divisao = sample.split(base$classe, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

# Accuray level
library(class)
i=1                          # declaration to initiate for loop
k.optm=1                     # declaration to initiate for loop
for (i in 1:40){ 
  knn.mod <-  knn(train = base_treinamento[, -7], test = base_teste[, -7], cl=base_treinamento[, 7], k=i)
  k.optm[i] <- 100 * sum(base_teste[, 7] == knn.mod)/NROW(base_teste[, 7])
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")  # to plot % accuracy wrt to k-value


library(class)
previsoes = knn(train = base_treinamento[, -7], test = base_teste[, -7],
                cl = base_treinamento[, 7], k = 10)
matriz_confusao = table(base_teste[,7], previsoes)
print(matriz_confusao)
library(caret)
confusionMatrix(matriz_confusao)

# ZeroR 
table(base_teste$classe)


# a diferença de acurácia para o outro arquivo em python se deve ao fato de não ter
# random_forest, enquanto aqui teve o seed. Lá o valor era aleatório e variava.

# A acurácia passou de 81% (1=k) para 87% (10=k)

