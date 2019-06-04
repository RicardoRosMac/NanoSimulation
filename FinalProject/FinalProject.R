library(plotrix)
library(parallel)

r <- 30 #radio exterior
ri <- 5 #radio interior
n <- 50 #numero de particulas
circle <- seq(0,360,0.005) #dibujado del circulo
cx <- r * cos(circle) #Circulo exterior
cy <- r * sin(circle)
cix <- ri * cos(circle) #circulo interior
ciy <- ri * sin(circle)

theta <- (pi*2) * runif(n) #angulos aleatorios
rad <- runif(n,ri,r) #radios aleatorios entre radio interior y radio exterior
xp <- rad * cos(theta) #coordenadas polares a cartesianas en X de las particulas
yp <- rad * sin(theta)#coordenadas polares a cartesianas en y de las particulas
a <- seq(0,2*pi-0.00001,2*pi/360) #Velocidad de la barra
xl <- r * cos(a) #coordenadas polares a cartesianas en X de la barra
yl <- r * sin(a) #coordenadas polares a cartesianas en y de la barra
#Particulas
#x = posicion en X, y = posicion en y, m = masa de la particulas
#radio = radio de giro, angulo = angulo direccion contraria al reloj, 
#vel = velocidad angular distinta para cada particula
p <- data.frame(x= xp, y = yp, m=runif(n,10,60),radio = rad, angulo = atan2(yp,xp),vel = (2*pi)/sample(180:450,length(xp)))
dens<-0.5 # Densidad
p$ram<-sqrt(p$m/(dens*pi))/5 #Radio de las pariculas con respecto a la masa

#clt <- makeCluster(detectCores() - 1)

for(i in 1:360) { #Pasos de la barra por frame
  png(paste("MovPar_d", i,".png", sep=""))
  plot(1, type="n", xlab="", ylab="", xlim=c(-r,r),ylim=c(-r,r), main = paste("angulo: ", i, sep = '') )
  lines(cy~cx,col="red") #circulo exterior
  lines(ciy~cix) #circulo interior
  segments(rep(0,length(xp)),rep(0,length(yp)),xl[i],yl[i],col="blue") #Barra 0 grados
  segments(rep(0,length(xp)),rep(0,length(yp)),-xl[i],-yl[i],col="blue") #Barra 180 grados
  for(j in 1:length(p$x)) {
    draw.circle(p$x[j],p$y[j],p$ram[j],col="red") #Dibujado de particulas
  }
  #actualizar coordenadas polares a cartesianas
  #calculo de la posicion de la particulas con respecto al angulo
  p$x <- p$radio * cos(p$angulo) 
  p$y <- p$radio * sin(p$angulo)
  p$angulo <- p$angulo + p$vel #Aumentar el angulo agregando velocidad angular
  graphics.off()
}

#stopCluster (clt)

#system("convert -delay 50 -size 300x300 MovPar_d*.png -loop 0 FDsPr.gif") # creamos animacion con ImageMagick

