#Simulacion de modelo VAR 3 
library(MASS)
library(ggplot2)
#install.packages("ggplot2")


# Observaciones
T<-c(100)
t<-c(1:T)
k<-c(3)
p<-c(2)

#Intercepto

V<-matrix(c(1.2,1.3,0.8), nrow = 3,ncol = 1, byrow = TRUE )

#Ruido del proceso
mu<-matrix(c(0,0,0), nrow = 3,ncol = 1, byrow = TRUE )
sigmaU<-matrix(c(0.8,0.3,0.4,0.3,0.9,0.4,0.4,0.4,0.7), nrow = 3,ncol = 3, byrow = TRUE )
U1 <- MASS::mvrnorm(n=T, mu = mu, Sigma = sigmaU)
U<-t(U1)
#Parametros y media del proceso
A1<-matrix(c(0.5,0,0,0.1,0.1,0.3,0,0.2,0.3),nrow = 3,ncol = 3,byrow = TRUE)
A2<-matrix(c(0.2,0,0,0.1,0.2,0.1,0.2,0.2,0.3),nrow = 3,ncol = 3,byrow = TRUE)
I<-diag(3)
miu<-solve((I-A1-A2))%*%V


#Generacion del proceso VAR 
Y<-matrix(c(0),nrow = 3,ncol = T,byrow = TRUE)
#y1=y2=miu
Y[,1:2]<-miu

for (i in 3:T) {
  for (w in 1:3)
  Y[w,i]<-V[w,] + A1[w,]%*%Y[,i-1]+A2[1,]%*%Y[,i-2]+U[w,i]
}

Y1<-t(Y)
matplot(Y1, type="l")
