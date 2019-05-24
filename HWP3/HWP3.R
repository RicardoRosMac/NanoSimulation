library(parallel)

pd <- read.csv("primes1.txt", sep="", header = FALSE) #Primos descargados
pd$V1 <- NULL
pd$V10 <- NULL
pdm <- as.matrix(pd)
primos <- as.vector(t(pdm)) 

hastA<-20011
desde<-5381
hasta<-11827
replicas<-30
primo <- function(n) {
  for (i in 2:(n-1)) {
    if ((n > i && n %% i) == 0) { # residuo es cero
      return(FALSE)
    }
  }
  return(n)
}
primos <- numeric() # un vector vacio
for (n in desde:hastA) {
  primos <- c(primos, primo(n)) # combinar vectores
}

noprimo <- function(n) {
  for (i in 2:(n-1)) {
    if ((n > i && n %% i) == 0) { # residuo es cero
      return(n)
    }
  }
  return(FALSE)
}
noprimos <- numeric() # un vector vacio
for (m in desde:hasta) {
  noprimos <- c(noprimos, noprimo(m)) # combinar vectores
}

PC<-primos[which(primos>0)] # Primos crecientes 
NPC<-noprimos[which(noprimos>0)] # No primos crecientes
PD<- sort(a,decreasing = TRUE) # Primos decrecientes 
NPD<-sort(l,decreasing = TRUE) # No primos decrecientes
PA<-sample(a) # Primos aleatorios
NPA<-sample(d) # No primos aleatorios

e <- sample(a, size=420 ,replace = TRUE, prob = NULL)
f<-sample(l,size=420, replace = TRUE,prob=NULL)

primx <- function(n) {
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores()-2))
PCt <- numeric()
PDt <-numeric()
PAt <- numeric()
NPDt <-numeric()
NPCt <-numeric()
NPAt <-numeric()

for (r in 1:replicas) {
  PCt <- c(at, system.time(foreach(n = a, .combine=c) %dopar% primx(n))[3]) # Primos crecientes
  PDt <- c(bt, system.time(foreach(n = b, .combine=c) %dopar% primx(n))[3]) # Primos decrecientes
  PAt <- c(vt, system.time(foreach(n = v, .combine=c) %dopar% primx(n))[3]) # Primos aleatorios
  NPDt <- c(dt, system.time(foreach(n = d, .combine=c) %dopar% primx(n))[3]) # No primos crecientes
  NPCt <- c(lt, system.time(foreach(n = l, .combine=c) %dopar% primx(n))[3]) # No primos decrecientes
  NPAt <- c(wt, system.time(foreach(n = w, .combine=c) %dopar% primx(n))[3]) # No primos aleatorios 
}
stopImplicitCluster()

mean(PCt)
mean(PDt)
mean(NPDt)
mean(NPCt)
mean(PAt)
mean(NPAt)
nucleos <- rep(1,20)
nucleos<-c(nucleos,rep(2,20))
digitos<-rep(5,10)
digitos<-c(digitos,rep(4,10))
digitos<-c(digitos,rep(3,10))
digitos<-c(digitos,rep(2,10))
digitos<-c(digitos,rep(1,10))
proportion<- rep(1,3)
proportion<-c(proportion,rep(2,3))
proportion<-c(proportion,rep(3,3))          

p<-c(1, 2, 3)
ordenamiento<-rep(p,18)
promedio<-(29.24632)
datos<-cbind(nucleos,digitos,proportion,ordenamiento,promedio)
datos<-as.data.frame(datos)

png("nucleos.png")
colores<-c("peach puff", "lemon chiffon", "mint cream", "navajo white", "gray")
boxplot(datos$promedio ~datos$nucleos, xlab="Núcleos", ylab="tiempo promedio", fill="datos$nucleos",  col=colores) 
png("digitos.png")
boxplot(datos$promedio ~datos$digitos, xlab="Número de dígitos", ylab="tiempo promedio", col=colores)
png("ordenamiento.png")
boxplot(datos$promedio ~datos$ordenamiento, xlab="Ordenamiento", ylab="tiempo promedio", col=colores)
png("proporcion.png")
boxplot(datos$promedio ~datos$proportion, xlab="Proporción", ylab="tiempo promedio", col=colores)
graphics.off()
