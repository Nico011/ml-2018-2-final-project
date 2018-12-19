#getwd()

setwd("D:\\utalca 2018-2\\Machine Learning\\final-project")
# delim2 para leer archivos de texto con formato "tab-separated values"
# http://www.sthda.com/english/wiki/reading-data-from-txt-csv-files-r-base-functions
tabla <- read.delim2("A.promedios.txt",
                    header = TRUE)

tabla <- na.omit(tabla)

dim(tabla)
# [1] 42 49

#tabla

#str(tabla)

# desordenamos el dataset
desordenado<-sample(1:42,size = 42, replace = FALSE)
datosdesorden <- tabla[desordenado,]

# sacamos el primer atributo (a predecir) y se lo asignamos a y
y<-datosdesorden[,1]
x<-datosdesorden[,-c(1)]
dim(x)
#length(y)
#class(x)
# [1] "data.frame"


#---------------- Normalización -------------------------------
#https://rpubs.com/sediaz/Normalize
#str(x)

norm <- as.data.frame(apply(x, 2, function(x) (x - min(x))/(max(x)-min(x))))
#str(norm)

#summary(norm)



#--------------- BoxPlot ---------------------------------------
#install.packages("mlbench")
#library("mlbench")

# outliers ------------

boxplot(norm)

# varlores atípicos superiores ----------------------------
listasup <- c() # esta lista contiene todos los outliers superiores, para despues contarlos

listasup <- c(listasup, which(norm$X4 > 0.8))
# [1]  6 19

listasup <- c(listasup, which(norm$X5 > 0.6))
# [1]  3 12 19 22 28 38 40

listasup <- c(listasup, which(norm$X6 > 0.8))
# [1]  4 11

listasup <- c(listasup, which(norm$X8 > 0.9))
# [1] 14

listasup <- c(listasup, which(norm$X14 > 0.9))
# [1]  3 12 22 26 28 38 40

listasup <- c(listasup, which(norm$X17 > 0.8))
# [1]  1  4 16 18 23 29 32 42

listasup <- c(listasup, which(norm$X20 > 0.8))
#[1]  1  4  5 24 29 32 42

listasup <- c(listasup, which(norm$X26> 0.9))
#[1] 19

listasup <- c(listasup, which(norm$X28> 0.8))
#[1] 8

summary(norm$X35)
0.5224 + 1.5*(0.5224-0.2985) # con esto calculo el valor máximo que no es atípico
# [1] 0.85825

listasup <- c(listasup, which(norm$X35> 0.85825))
# [1] 18 26

summary(norm$X36)
0.625 + 1.5*(0.625-0.4375)
# [1] 0.90625

listasup <- c(listasup, which(norm$X36> 0.90625))
# [1] 18 34

summary(norm$X41)
0.565 + 1.5*(0.565-0.3268)
# [1] 0.9223

listasup <- c(listasup, which(norm$X41 > 0.9223))
# [1] 17 30

summary(norm$X42)
0.6 + 1.5*(0.6-0.385) 
# [1] 0.9225

listasup <- c(listasup, which(norm$X42 > 0.9225))
# [1] 17 30

listasup <- c(listasup, which(norm$X43 > 0.8))
# [1]  1  5  7 13 17 19 21 24 27 29

listasup <- c(listasup, which(norm$X44 > 0.6))
# [1] 13 19 21 24 29 30

listasup <- c(listasup, which(norm$X45 > 0.6))
# [1] 11 13 19 21 24 29 30 33

listasup

t1 <- table(listasup)
t1
# el mejor candidato a eliminar se repite en 6 columnas

# valores atípicos inferiores ------------------------------
listainf <- c()

listainf<- c(listainf, which(norm$X4 < 0.4))
# [1]  3  7 12 22 26 28 38 39 40

listainf<- c(listainf, which(norm$X5 < 0.2))
# [1]  6  7 39

listainf <- c(listainf, which(norm$X11 < 0.9))
# [1] 6 7

listainf <- c(listainf, which(norm$X17 < 0.4))
# [1]  3  7  9 10 13 14 20 28 38

listainf <- c(listainf, which(norm$X24 < 0.2))
# [1] 32 42

listainf <- c(listainf, which(norm$X25 < 0.2))
# [1] 22

listainf <- c(listainf, which(norm$X26 < 0.1))
# [1] 39

listainf <- c(listainf, which(norm$X32 < 0.1))
# [1] 26

summary(norm$X36)
0.4375 - 1.5*(0.625-0.4375)
# [1] 0.15625

listainf <- c(listainf, which(norm$X36 < 0.15625))
# [1]  3 22 26 38 40

summary(norm$X42)
0.3850 - 1.5*(0.6 - 0.385)
# [1] 0.0625
listainf <- c(listainf, which(norm$X42 < 0.0625))
# [1]  1 29

listainf <- c(listainf, which(norm$X45 < 0.2))
# [1] 10

t2 <- table(listainf)
t2
# el mejor candidato a eliminar se repite en 4 columnas


# ya tenemos todos los valores atípicos superiores e inferiores
# veremos cuáles se repiten más para eliminarlos
# pero...
# solo podemos eliminar el 5% de los datos
42*0.05
# [1] 2.1
# es decir 2


lista <- c(listainf, listasup)
t3 <- table(lista)
t3

# analizando los outliers superiores e inferiores en conjunto, hay 2 elementos 
# que se repiten en 6 columnas, por lo que serían esos 2 los más indicados para
# ser eliminados

newnorm <- norm[-c(17, 28), ]
newnorm <- as.data.frame(apply(newnorm, 2, function(newnorm) (newnorm - min(newnorm))/(max(newnorm)-min(newnorm))))
# dim(newnorm)
boxplot(newnorm)

plot(newnorm)

# LOF algorithm
#comando cor()


library(DMwR)
# install.packages("DMwR")

# LOF
norm1 <- norm[,]
dim(norm1)
outlier.scores <- lofactor(norm1, k=4)
plot(density(outlier.scores))
outliers <- order(outlier.scores, decreasing=T)[1:5]

print(outliers)

# ??????????????????
n <- nrow(norm)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(norm), cex=.8, xlabs = labels)


#cook's distance
mod <- lm(X1~., data=x)
cooksd <- cooks.distance(mod)

plot(cooksd, pch="*", cex=2, ylim = c(0,1000), main = "hola")
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels



