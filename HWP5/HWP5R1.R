inicio <- -0.1
final <- -inicio
pi <- 3.14159265359
muestra <- c(10,50,100,500,1000) # Tamaño de muestra
repeticiones <- 30

calcpi=function() {
  xs <- runif(replicas, min= inicio, max= final)
  ys <- runif(replicas, min= inicio, max= final)
  in.circle <- xs^2 + ys^2 <= inicio^2
  mc.pi <- (sum(in.circle)/replicas)*4
  return(mc.pi) 
}

suppressMessages(library(doParallel)) 
registerDoParallel(makeCluster(detectCores() - 1)) 

resultados=data.frame() 

for(replicas in muestra) { 
  for(i in 1:repeticiones) { 
    montecarlo <- foreach(i = 1:300, .combine=c) %dopar% calcpi() 
    mc.pi=sum(montecarlo)/300
    diferencia=(pi-mc.pi)/(pi*100) 
    pii <- pi
    resultados=rbind(resultados,c(replicas,i,pii,mc.pi,diferencia))
  }
}

names(resultados)=c("muestra","replicas","Valor de Pi","aprox.pi","error") 

png("Error.png") 
colores<-c("peach puff", "navajo white", "lemon chiffon", "mint cream")
boxplot(data=resultados,error~muestra,xlab="Tamaño de la muestra",ylab="Tamaño del error", col=colores)
abline(h=0, col="red", pch=15)
graphics.off()

png("Aprox.png") 
colores<-c("peach puff", "navajo white", "lemon chiffon", "mint cream")
boxplot(data=resultados,aprox.pi~muestra,xlab="Tamaño de la muestra",ylab="Aproximación de Pi", col=colores)
abline(h=pi, col="red", pch=15)
points(x=500, y=pi, col=4)
graphics.off()
