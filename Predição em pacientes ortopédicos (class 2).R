base = read.csv("ortopedic_2C.csv")

summary (base)

str(base)

names(base)[names(base)=="class"]<-"classe" 

base$classe = factor(base$classe, levels = c('Normal', 'Abnormal'), labels = c(0,1))

# ML - KNN
base[, 1:6] = scale(base[, 1:6])

library(caTools)
set.seed(1)
divisao = sample.split(base$classe, SplitRatio = 0.70)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

# Accuracy level
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
                cl = base_treinamento[, 7], k = 31)
matriz_confusao = table(base_teste[,7], previsoes)
print(matriz_confusao)
library(caret)
confusionMatrix(matriz_confusao)

# ZeroR 
table(base_teste$classe)

# A acurácia passou de 83% (1=k) para 88% (31=k)

