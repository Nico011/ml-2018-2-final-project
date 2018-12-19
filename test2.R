setwd("D:\\utalca 2018-2\\Machine Learning\\final-project")

tabla2 <- read.delim2("a.txt",
                     header = TRUE)

tabla2 <- na.omit(tabla2)

dim(tabla2)

y2<-datosdesorden[,1]
x2<-datosdesorden[,-c(1)]

norm2 <- as.data.frame(apply(x2, 2, function(x2) (x2 - min(x2))/(max(x2)-min(x2))))

df1 = cor(norm2[,1:10])
df1

df2 = cor(tabla2[,32:41])
df2
