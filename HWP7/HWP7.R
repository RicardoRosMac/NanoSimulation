library(reshape2) 
library(lattice)
library(parallel)

g <- function(x, y) {
  return (((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

low <- -3
high <- -low
step <- 0.25
replicas <- 100

replica <- function(t){
  puntosxy<- c()
  curr <- c( x = runif(1, min = low, max = high), y = runif(1, min = low, max = high))
  best <- curr
  for (tiempo in 1:t) {
    delta <- runif(1, 0, step)
    left <- curr + c(-delta,0) # Eje izquierdo
    right <- curr + c(delta,0) # Eje Derecho
    up <- curr + c(0,-delta) # Eje arriba
    down <- curr + c(0,delta) # Eje abajo
    puntos <- c(left, right, up, down)
    
    for(k in 1:8){
      if(puntos[k] < (-3)){
        puntos[k] <- puntos[k]+3 
      }
      if(puntos[k] > 3){
        puntos[k] <- puntos[k]-3
      }
    }
    vecx <- c()
    vecy <- c()
    for(p in 1:8){
      if(p %% 2 == 0){
        vecy <- c(vecy,puntos[p])
      }else{
        vecx <- c(vecx,puntos[p])
      }
    }
    valg <- c()
    for(q in 1:4){
      valg <- c(valg, g(vecx[q], vecy[q]) )
    }
    dm <- which.max(valg)
    curr <- c(vecx[dm], vecy[dm])
    puntosxy <- c(puntosxy, vecx[dm],vecy[dm])
  }
  return(puntosxy)
}

resultado <- c()
for(q in 1:5){
  resultado <- c(resultado, replica(100))
}

vx <- c()
vy <- c()
for(p in 1:1000){
  if(p %% 2 == 0){
    vy <- c(vy,resultado[p])
  }else{
    vx <- c(vx,resultado[p])
  }
}

vx1 <- c(vx[1:100])
vx2 <- c(vx[101:200])
vx3 <- c(vx[201:300])


vy1 <- c(vy[1:100])
vy2 <- c(vy[101:200])
vy3 <- c(vy[201:300])
vy4 <- c(vy[301:400])
vy5 <- c(vy[401:500])

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
for(j in 1:100){
  x <- seq(-6, 5, 0.25)
  y <-  x
  z <- outer(x, y, g)
  dimnames(z) <- list(x, y)
  d <- melt(z)
  names(d) <- c("x", "y", "z")
  if(j < 10){
    nombre <-  paste0("P_100", j, ".png", sep="")
  }else if(j>= 10 & j < 100){ 
    nombre <-  paste0("P_10", j, ".png", sep="") }else{
      nombre <-  paste0("P_1", j, ".png", sep="")
    }
  png(nombre, width=500, height=500)
  plot(levelplot(z ~ x * y, data = d))
  trellis.focus("panel", 1, 1, highlight=FALSE)
  lpoints(vx1[j], vy1[j], pch=19, col="blue", cex=1)
  trellis.unfocus()
  trellis.focus("panel"[1], 1, 1, highlight=FALSE)
  lpoints(vx2[j], vy2[j], pch=19, col="red", cex=1)
  trellis.unfocus()
  trellis.focus("panel"[1], 1, 1, highlight=FALSE)
  lpoints(vx3[j], vy3[j], pch=19, col="green", cex=1)
  trellis.unfocus()
  trellis.focus("panel"[1], 1, 1, highlight=FALSE)
  lpoints(vx4[j], vy4[j], pch=19, col="orange", cex=1)
  trellis.unfocus()
  trellis.focus("panel"[1], 1, 1, highlight=FALSE)
  lpoints(vx5[j], vy5[j], pch=19, col="yellow", cex=1)
  trellis.unfocus()
  graphics.off() 
}
system("convert -delay 50 -size 300x300 P_*.png -loop 0 Busloc.gif") # creamos animacion con ImageMagick


