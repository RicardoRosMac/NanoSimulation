library(parallel)
library(ggplot2) 

dat <- data.frame() 
pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}

poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

domin.by <- function(target, challenger, total) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}

verify <- function(i){
  val <- c()
  for (j in 1:k) { # Se evaluan los objetivos (para evaluar las soluciones -> for (i in 1:n) #})
    val <- c(val, eval(obj[[j]], sol[i,], tc))
  }
  return(val)
}

prop <- function(i){
  return(list(poli(vc, md, tc)))
}

cluster <- makeCluster(detectCores()- 1)
clusterExport(cluster, c("domin.by", "sign", "eval", "poli", "pick.one"))

vc <- 4
md <- 3
tc <- 5
funciones <- seq(2, 14, by=1)
for (k in funciones) {
  for (replicas in 1:30) {
    obj <- list()
    clusterExport(cluster, c("vc", "md", "tc"))
    obj <- parSapply(cluster, 1:k, prop)
    minim <- (runif(k) > 0.5)
    sign <- (1 + -2 * minim)
    n <- 200 # cuantas soluciones aleatorias
    sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
    val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
    clusterExport(cluster, c("tc", "obj", "sol", "eval", "k", "n"))
    val <- parSapply(cluster, 1:n, verify)
    val <- t(val)

    mejor1 <- which.max(sign[1] * val[,1])
    mejor2 <- which.max(sign[2] * val[,2])
    cual <- c("max", "min")
    xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
    yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
    png("p11_init.png")
    plot(val[,1], val[,2], xlab=xl, ylab=yl, main="Ejemplo bidimensional")
    graphics.off()
    png("p11_mejores.png")
    plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
         ylab=paste(yl,"mejor con bolita naranja"),
         main="Ejemplo bidimensional")
    points(val[mejor1, 1], val[mejor1, 2], col="blue", pch=15, cex=1.5)
    points(val[mejor2, 1], val[mejor2, 2], col="orange", pch=16, cex=1.5)
    graphics.off()
    no.dom <- logical()
    dominadores <- integer()
    for (i in 1:n) {
      d <- logical()
      for (j in 1:n) {
        d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
      }
      cuantos <- sum(d)
      dominadores <- c(dominadores, cuantos)
      no.dom <- c(no.dom, cuantos == 0) # nadie le domina
    }
    frente <- subset(val, no.dom) # solamente las no dominadas
    dat <- rbind(dat, c(k, replicas, sum(no.dom)/n))
    vec <- seq(1, k, by = 1)
    for (i in vec) {
      if((i+1) == is.element(i+1, vec)*(i+1)){
        png(paste("p11_frente", k, "_", replicas, "_", i, "-", i+1, ".png", sep=""))
        xt = paste("Objetivo", i, "(", cual[minim[i] + 1], ")", sep = "")
        yt = paste("Objetivo", i+1, "(", cual[minim[i+1] + 1], ")", sep = "")
        plot(val[,i], val[,i+1], xlab=paste(xt, "mejor con cuadro azul"),
             ylab=paste(yt,"mejor con bolita naranja"),
             main="Ejemplo bidimensional")
        points(frente[,i], frente[,i+1], col="green", pch=16, cex=1.5)
        graphics.off()
      }
    }
    graphics.off()
    
    data <- data.frame(pos=rep(0, n), dom=dominadores)
    png(paste("p11_violin", k, "_", replicas, ".png", sep = ""))
    gr <- ggplot(data, aes(x=pos, y=dom)) + geom_violin(fill="orange", color="red")
    print(gr + geom_boxplot(width=0.2, fill="blue", color="white", lwd=2) +
            xlab("") +
            ylab("Frecuencia") +
            ggtitle("Cantidad de soluciones dominantes"))
    graphics.off()
  }
}

names(dat) <- c("k", "Replica", "Porcentaje")
dat$k <- as.factor(dat$k)
setEPS()
postscript("numdfunobjetiva.eps")
por <- ggplot(dat, aes(x=k, y=Porcentaje, fill=k)) + geom_violin(scale = "width") + scale_fill_brewer(palette="Paired")+ theme_minimal() + geom_boxplot(inherit.aes = TRUE, fill="gray", color="brown", width = 0.1)
print(por)
graphics.off()

a <- shapiro.test(dat$Porcentaje)
a$p.value
a$p.value < 0.05
b <- kruskal.test(dat$Porcentaje~dat$k)
b$p.value
b$p.value < 0.05
c <- pairwise.wilcox.test(dat$Porcentaje, dat$k, p.adjust.method = "none")
c$p.value
c$p.value < 0.05
