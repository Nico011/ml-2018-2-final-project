#getwd()

setwd("D:\\utalca 2018-2\\Machine Learning\\final-project")
# delim2 para leer archivos de texto con formato "tab-separated values"
# http://www.sthda.com/english/wiki/reading-data-from-txt-csv-files-r-base-functions
tabla <- read.delim2("A.promedios.txt",
                    header = TRUE)

# por alguna razón, el read.delim2 rellena la tabla con NAs, así que los eliminamos
tabla <- na.omit(tabla)

# dim(tabla)
# [1] 42 49

#tabla

#str(tabla)

# desordenamos el dataset (esto lo haremos despues de eliminar outliers)
# desordenado<-sample(1:42,size = 42, replace = FALSE)
# datosdesorden <- tabla[desordenado,]

# y<-tabla[,1]
# x<-tabla[,-c(1)]
# dim(x)
#length(y)
#class(x)
# [1] "data.frame"


#---------------- Normalización -------------------------------
#https://rpubs.com/sediaz/Normalize
#str(x)

norm <- as.data.frame(apply(tabla, 2, function(tabla) (tabla - min(tabla))/(max(tabla)-min(tabla))))
#str(norm)

# which(is.na(norm))
# integer(0) # no hay NAs

summary(norm)

# BoxPlot ---------------------------------------
#install.packages("mlbench")
#library("mlbench")

# outliers

boxplot(norm)

# varlores atípicos superiores
listasup <- c() # esta lista contiene todos los outliers superiores, para despues contarlos

listasup <- c(listasup, which(norm$X4 > 0.8))

listasup <- c(listasup, which(norm$X5 > 0.6))

listasup <- c(listasup, which(norm$X6 > 0.8))

listasup <- c(listasup, which(norm$X8 > 0.9))

listasup <- c(listasup, which(norm$X14 > 0.9))

listasup <- c(listasup, which(norm$X17 > 0.8))

listasup <- c(listasup, which(norm$X20 > 0.8))

listasup <- c(listasup, which(norm$X26> 0.9))

listasup <- c(listasup, which(norm$X28> 0.8))

summary(norm$X35)
0.5224 + 1.5*(0.5224-0.2985) # con esto calculo el valor máximo que no es atípico
# [1] 0.85825

listasup <- c(listasup, which(norm$X35> 0.85825))

summary(norm$X36)
0.625 + 1.5*(0.625-0.4375)
# [1] 0.90625

listasup <- c(listasup, which(norm$X36> 0.90625))

summary(norm$X41)
0.565 + 1.5*(0.565-0.3268)
# [1] 0.9223

listasup <- c(listasup, which(norm$X41 > 0.9223))

summary(norm$X42)
0.6 + 1.5*(0.6-0.385) 
# [1] 0.9225

listasup <- c(listasup, which(norm$X42 > 0.9225))

listasup <- c(listasup, which(norm$X43 > 0.8))

listasup <- c(listasup, which(norm$X44 > 0.6))

listasup <- c(listasup, which(norm$X45 > 0.6))

listasup

t1 <- table(listasup)
t1
# el mejor candidato a eliminar se repite en 6 columnas (35)

# valores atípicos inferiores
listainf <- c()

listainf<- c(listainf, which(norm$X4 < 0.4))

listainf<- c(listainf, which(norm$X5 < 0.2))

listainf <- c(listainf, which(norm$X11 < 0.9))

listainf <- c(listainf, which(norm$X17 < 0.4))

listainf <- c(listainf, which(norm$X24 < 0.2))

listainf <- c(listainf, which(norm$X25 < 0.2))

listainf <- c(listainf, which(norm$X26 < 0.1))

listainf <- c(listainf, which(norm$X32 < 0.1))

summary(norm$X36)
0.4375 - 1.5*(0.625-0.4375)
# [1] 0.15625

listainf <- c(listainf, which(norm$X36 < 0.15625))

summary(norm$X42)
0.3850 - 1.5*(0.6 - 0.385)
# [1] 0.0625
listainf <- c(listainf, which(norm$X42 < 0.0625))

listainf <- c(listainf, which(norm$X45 < 0.2))

t2 <- table(listainf)
t2
# el mejor candidato a eliminar se repite en 4 columnas (42)


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
# ser eliminados (5 y 35)

newnorm1 <- norm[-c(5, 35), ]

# dim(newnorm)
# [1] 40 49

# normalizamos el dataset sin los outliers
newnorm1 <- as.data.frame(apply(newnorm1, 2, function(newnorm1) (newnorm1 - min(newnorm1))/(max(newnorm1)-min(newnorm1))))

boxplot(newnorm1)

# al normalizar apararecen columnas con valores discretos (0 o 1), veamos la correlación entre
# estas columnas (X11, X12, X13, X14, X20, X21 y X43)

atrs <- c("X11", "X12", "X13", "X14", "X20", "X21", "X43")
subt <- newnorm1[atrs]
cor1 = cor(subt)
cor1
# la mayor correlación es de 0.7478159 (X14 con X21)


# LOF algorithm ----------------------------
# https://rpubs.com/maulikpatel/228336

library(DMwR)
install.packages("DMwR")

# LOF
norm1 <- norm[,-c(1)]
outlier.scores <- lofactor(norm1, k=5)
plot(density(outlier.scores))
outliers <- order(outlier.scores, decreasing=T)[1:5]

print(outliers)
# [1] 42 35 37 13 38

# según el algoritmo LOF los outliers son 42 y 35


newnorm2 <- norm[-c(35, 42), ]

newnorm2 <- as.data.frame(apply(newnorm2, 2, function(newnorm2) (newnorm2 - min(newnorm2))/(max(newnorm2)-min(newnorm2))))


# comparemos ambos métodos de eliminación de outliers con sus respectivos boxplot
boxplot(newnorm1, main = "my boxplot")
boxplot(newnorm2, main = "LOF")


# Algoritmo de Boruta ---------------------------

library(Boruta)
# install.packages("Boruta")

set.seed(111)
boruta.train1 <- Boruta(Y~. , data = newnorm1, doTrace = 2)
print(boruta.train1)
# Boruta performed 99 iterations in 5.19514 secs.
#  10 attributes confirmed important: X1, X10, X19, X27, X31 and 5 more;
#  35 attributes confirmed unimportant: X11, X12, X13, X14, X15 and 30 more;
#  3 tentative attributes left: X38, X5, X8;

# tenemos nuestro resultado inicial, pero se puede refinar más para saber si los atributos "tentativos"
# son o no importantes

boruta.final1 <- TentativeRoughFix(boruta.train1)
print(boruta.final1)
# Boruta performed 99 iterations in 5.19514 secs.
#  Tentatives roughfixed over the last 99 iterations.
#  10 attributes confirmed important: X1, X10, X19, X27, X31 and 5 more;
#  38 attributes confirmed unimportant: X11, X12, X13, X14, X15 and 33 more;

# finalmente el algoritmo nos entrega solo 10 atributos importantes

boruta.final1$finalDecision

# estos son: X1, X7, X10, X19, X27, X31, X33, X46, X47, X48
# usando el método de eliminación de outliers con boxplot y boruta




set.seed(111)
boruta.train2 <- Boruta(Y~. , data = newnorm2, doTrace = 2)
print(boruta.train2)
# Boruta performed 99 iterations in 5.208388 secs.
#  12 attributes confirmed important: X1, X10, X19, X27, X31 and 7 more;
#  31 attributes confirmed unimportant: X11, X12, X13, X14, X15 and 26 more;
#  5 tentative attributes left: X30, X32, X42, X5, X8;

# tenemos nuestro resultado inicial, pero se puede refinar más para saber si los atributos "tentativos"
# son o no importantes

boruta.final2 <- TentativeRoughFix(boruta.train2)
print(boruta.final2)
# Boruta performed 99 iterations in 5.208388 secs.
# Tentatives roughfixed over the last 99 iterations.
#  13 attributes confirmed important: X1, X10, X19, X27, X31 and 8 more;
#  35 attributes confirmed unimportant: X11, X12, X13, X14, X15 and 30 more;

boruta.final2$finalDecision

# estos son: X1, X7, X10, X19, X27, X31, X33, X37, X38, X42, X46, X47, X48
# usando el método de eliminación de outliers con LOF y boruta



# MLR usando todos los atributos --------------------------------
lm.y <- lm(Y ~ . , data = norm[,1:49])
summary(lm.y)
# Residuals:
#   ALL 42 residuals are 0: no residual degrees of freedom!
#   Coefficients: (7 not defined because of singularities)



# MLR quitando outliers (1) -----------------------------
lm.y1 <- lm(Y ~ . , data = newnorm1[, 1:49])
summary(lm.y1)
# Residuals:
#   ALL 40 residuals are 0: no residual degrees of freedom!
#   Coefficients: (9 not defined because of singularities)

# MLR quitando outliers (2) ---------------------------
lm.y2 <- lm(Y ~ . , data = newnorm2[, 1:49])
summary(lm.y2)
# Residuals:
#   ALL 40 residuals are 0: no residual degrees of freedom!
#   Coefficients: (9 not defined because of singularities)

# no funciona?


# MLR usando los atributos considerados importantes por Boruta (10 cols) -------------
tabla1 <- newnorm1[,c("Y", "X1", "X7", "X10", "X19", "X27", "X31", "X33", "X46", "X47", "X48")]
lm.y3 <- lm(Y ~ ., data = tabla1[,])
summary(lm.y3)
# Residuals:
#     Min       1Q   Median       3Q      Max 
#-0.39102 -0.12262  0.02563  0.11879  0.29406 
# Residual standard error: 0.1951 on 29 degrees of freedom
# Multiple R-squared:  0.624,	Adjusted R-squared:  0.4943 
# F-statistic: 4.812 on 10 and 29 DF,  p-value: 0.0004255


# MLR usando los atributos considerados importantes por Boruta (13 cols) ------------------------
tabla2 <- newnorm2[,c("Y", "X1", "X7", "X10", "X19", "X27", "X31", "X33", "X37", "X38", "X42", "X46", "X47", "X48")]
lm.y4 <- lm(Y ~., data = tabla2[,])
summary(lm.y4)
# Residuals:
#     Min       1Q   Median       3Q      Max 
#-0.37785 -0.14048 -0.00371  0.12711  0.33659 
# Residual standard error: 0.2107 on 26 degrees of freedom
# Multiple R-squared:  0.6228,	Adjusted R-squared:  0.4342 
# F-statistic: 3.302 on 13 and 26 DF,  p-value: 0.004608

# si nos fijamos en los R^2 podemos notar que el modelo no está muy bien ajustado a los datos
# sin embargo, pareciera que el mejor modelo es el de 10 columnas (49.4%)


# Backward elimination -----------------------------------
# analizamos el modelo para eliminar columnas que no aportan a la predicción
anova(lm.y3)
# X33 sum sq: 0.0068 es el menor de todos

lm2.y3 <- update(lm.y3, . ~ . - X33)
summary(lm2.y3)
# vemos que el modelo se ajustó a 50.4%)

# aplicamos de nuevo para que R continúe el análisis
final.lm <- step(lm.y3)

summary(final.lm)
# Residuals:
#     Min       1Q   Median       3Q      Max 
#-0.45820 -0.12326  0.03564  0.11240  0.27310 
# Residual standard error: 0.1845 on 34 degrees of freedom
# Multiple R-squared:  0.6059,	Adjusted R-squared:  0.548 
# F-statistic: 10.46 on 5 and 34 DF,  p-value: 3.887e-06

#finalmente podemos ver que el modelo se ajustó hasta 55% aprox
#usando las columnas X7, X33, X46, X47, X48


predY <- predict(final.lm, tabla1 <- newnorm1[,c("Y", "X7", "X33", "X46", "X47", "X48")])


# install.packages("hydroGOF")
library(hydroGOF)

RMSE = rmse(predY, tabla1$Y)
RMSE
# [1] 0.1700546


# Support Vector Regression----------------------------------------------------------------
# https://www.kdnuggets.com/2017/03/building-regression-models-support-vector-regression.html

install.packages("e1071")
library(e1071)

# probaremos la regresión con 3 casos 
  # (1) datos sin eliminación de outliers (49cols, 42obs)
  # (2) datos luego de aplicar boruta para la regresión lineal (11cols, 40obs)
  # (3) datos luego de aplicar backward elimination (6cols, 40obs)

#(1)
svm1 <- svm(Y~., norm)
svm1
predictYsvm1 <- predict(svm1,norm)
predictYsvm1









