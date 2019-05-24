library(testit)
library(magick)
library(ggplot2)
library(parallel)

n <- 50
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n),m=rnorm(n))
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
p$m <- abs(p$m)
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
p$g<-as.factor(p$g)
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)

ggplot() +
  geom_point(data=p, aes(x = p$x, y= p$y,size=p$m,color=p$g))+
  scale_colour_manual(values=colores)+ 
  ggtitle("Partículas generadas")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(size=FALSE,color=guide_legend(title="Carga"))+
  scale_x_continuous(name="x",limits = c(0, 1))+
  scale_y_continuous(name="y", limits = c(0, 1))+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        plot.title=element_text(size=14),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12))
# ggsave("Particulas_generadas.png")


eps <- 0.001
fuerza <- function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  mi <- p[i,]$m
  fx <- 0
  fy <- 0
  for (j in 1:n) {
    cj <- p[j,]$c
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
    
    fx <- fx - dx * factor
    fy <- fy - dy * factor
  }
  return(c(fx, fy))
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
system("rm -f p9_t*.png") # borramos anteriores en el caso que lo hayamos corrido
tmax <- 100

ggplot() +
  geom_point(data=p, aes(x = p$x, y= p$y,size=p$m,color=p$g))+
  scale_colour_manual(values=colores)+ 
  ggtitle("Estado inicial")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(size=FALSE,color=guide_legend(title="Carga"))+
  scale_x_continuous(name="x",limits = c(0, 1))+
  scale_y_continuous(name="y", limits = c(0, 1))+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        plot.title=element_text(size=14),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12))
ggsave("p9_t0.png")
datos<-data.frame()

for (iter in 1:tmax) {
  
  pi<-cbind(p$x,p$y)
  pi<-data.frame(pi)
  colnames(pi)<-c("x","y")
  
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
  delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * (f[c(TRUE, FALSE)][i]/p[i,]$m), 1), 0)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * (f[c(FALSE, TRUE)][i]/p[i,]$m), 1), 0)
  
  disx<- pi$x - p$x
  disy<- pi$y - p$y
  v<-sqrt(disx^2 + disy^2)
  res<-cbind(p$m,v)
  datos<-rbind(datos,res)
  
  
  ggplot() +
    geom_point(data=p, aes(x = p$x, y= p$y,size=p$m,color=p$g))+
    scale_x_continuous(name="x",limits = c(0, 1))+
    scale_y_continuous(name="y", limits = c(0, 1))+
    scale_colour_manual(values=colores)+  
    ggtitle(paste("Paso",iter))+
    theme(plot.title = element_text(hjust = 0.5))+
    guides(size=FALSE,color=guide_legend(title="Carga"))+
    theme(axis.text.x=element_text(size=12),
          axis.text.y=element_text(size=12),
          plot.title=element_text(size=14),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12))
  ggsave(paste("P9_t",iter,".png", sep=""))
  
}
stopImplicitCluster()

for(h in 1:tmax){
  png(paste("histo", h,  ".png", sep=""))
  hist(main = paste("", h), as.numeric(datos[h,]), xlab = "Distancia recorrida", ylab = "Frecuencia", border="blue", 
       col="gray")
  graphics.off()
}

datos$n<-seq(1,n,1)
colnames(datos)<-c("Masa","Velocidad","n")
tabla<-data.frame()

for (i in 1:n){
  res<-datos[datos$n==i,]
  resultados<-cbind(res[i,]$Masa,mean(res$Velocidad))
  tabla<-rbind(tabla,resultados)
}
colnames(tabla)<-c("Masa","Velocidad")

ggplot(tabla, aes(x=Masa, y=Velocidad))+
  geom_point(shape = 17, color = "#FC4E07", size = 1.7)+
  geom_smooth(method = "lm", formula =y ~log(x))+
  scale_x_continuous(name="Masa")+
  scale_y_continuous(name="Velocidad")
ggsave("Veldep.png")

tabla$Carga<-p$c
summary(lm(tabla$Velocidad ~ log(tabla$Masa), data = tabla))

system("convert -delay 50 -size 300x300 p9_t*.png -loop 0 p9mov.gif") # creamos animacion con ImageMagick
