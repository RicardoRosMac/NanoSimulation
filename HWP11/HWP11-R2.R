library(parallel)
library(testit)

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

verify <- function(i){
  val <- matrix(rep(NA, k), ncol=k)
  for (j in 1:k) { # para todos los objetivos
    val[, j] <- eval(obj[[j]], sol[i,], tc)
  }
  return(val)
}

domin.by <- function(target, challenger, total) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}

conquered <- function(i){
  d <- logical()
  for (j in 1:n) {
    d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
  }
  return(d)
}

vc <- 4
md <- 3
tc <- 5
k <- 2 # cuantas funciones objetivo
obj <- list()
for (i in 1:k) {
  obj[[i]] <- poli(md, vc, tc)
}
minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim)
n <- 200 # cuantas soluciones aleatorias
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
val <- matrix(rep(NA, k * n), nrow=n, ncol=k)

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "verify")
clusterExport(cluster, "eval")
clusterExport(cluster, "obj")
clusterExport(cluster, "sol")
clusterExport(cluster, "tc")
clusterExport(cluster, "k")
clusterExport(cluster, "n")

val <- parSapply(cluster, 1:n, verify)
val <- t(val)
stopCluster(cluster)

mejor1 <- which.max(sign[1] * val[,1])
mejor2 <- which.max(sign[2] * val[,2])
cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")

no.dom <- logical()
conqueredres <- integer()

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "conquered")
clusterExport(cluster, "domin.by")
clusterExport(cluster, "val")
clusterExport(cluster, "sign")
clusterExport(cluster, "k")
clusterExport(cluster, "n")

d <- parSapply(cluster, 1:n, conquered)
stopCluster(cluster)

for(x in 1:n){
  cuantos <- sum(d[,x])
  conqueredres <- c(conqueredres, cuantos)
  no.dom <- c(no.dom, cuantos == 0) # nadie le domina
}
frente <- subset(val, no.dom) # solamente las no dominadas

png(paste0("p11_frenteReto2.png"))
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"))
points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
graphics.off()

mutacion <- function(sol, vc) {
  pos <- sample(1:vc, 1)
  mut <- sol
  delta <- 0.1
  mut[pos] <- (sol[pos]) * delta
  return(mut)
}

muta <- function(i){
  if (runif(1) < pm) {
    return(mutacion(sol[i,], vc))
  }
  else{
    return(sol[i,])
  }
}

reproduccion <- function(x, y, vc) { #Reprodución
  pos <- sample(2:(vc-1), 1)
  xy <- c(x[1:pos], y[(pos+1):vc])
  yx <- c(y[1:pos], x[(pos+1):vc])
  return(c(xy, yx))
}


cluster <- makeCluster(detectCores() - 1)

pm <- 0.05 # Probabilidad de mutación
rep <- 50 # Cantidad de veces que se realiza reproducción
tmax <- 100 # Cantidad de generaciones
for (iter in 1:tmax) { 
  
  clusterExport(cluster, "pm")
  clusterExport(cluster, "vc")
  clusterExport(cluster, "sol")
  clusterExport(cluster, "mutacion")
  clusterExport(cluster, "muta")
  sol <- t(parSapply(cluster, 1:n,muta)) #Mutación

  for (i in 1:rep) { # Reproducción
    padres <- sample(1:n, 2, replace=FALSE)
    hijos <- reproduccion(sol[padres[1],], sol[padres[2],], vc)
    sol <- rbind(sol, hijos[1:vc]) # primer hijo
    sol <- rbind(sol, hijos[(vc+1):(2*vc)]) # segundo hijo
  }
  val <- matrix(rep(NA, k * nrow(sol)), nrow=nrow(sol), ncol=k)
  clusterExport(cluster, "verify")
  clusterExport(cluster, "eval")
  clusterExport(cluster, "obj")
  clusterExport(cluster, "sol")
  clusterExport(cluster, "tc")
  clusterExport(cluster, "k")
  clusterExport(cluster, "n")
  val <- parSapply(cluster, 1:nrow(sol), verify)
  val <- t(val)
  no.dom <- logical()
  dom <- logical()
  conqueredres <- integer()
  clusterExport(cluster, "conquered")
  clusterExport(cluster, "domin.by")
  clusterExport(cluster, "val")
  clusterExport(cluster, "sign")
  clusterExport(cluster, "k")
  clusterExport(cluster, "n")
  
  d <- parSapply(cluster, 1:nrow(sol), conquered)
  for(x in 1:nrow(sol)){
    cuantos <- sum(d[,x])
    conqueredres <- c(conqueredres, cuantos)
    no.dom <- c(no.dom, cuantos == 0) # nadie le domina
    dom <- c(dom, cuantos != 0)
  }
  frente_sol <- subset(sol, no.dom) # solamente las no dominadas
  dominadas <- subset(sol, dom)
  frente <- subset(val, no.dom) # solamente las no dominadas
  domi <- order(conqueredres)
  domi <- domi[1:n]
  
  sol <- sol[domi,]
  val <- val[domi,]
  digitos <- floor(log(tmax, 10)) + 1
  tl <- paste0(iter, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  
  png(paste0("Genetico",tl,".png"))
  plot(val[,1], val[,2], xlab=xl, ylab=yl, main=paste("Generación",iter))
  points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
  graphics.off()
  
  if(nrow(frente) == n)
  {
    break;
  }
}
stopCluster(cluster)
system("convert -delay 50 -size 300x300 Genetico*.png -loop 0 Gen.gif") # creamos animacion con ImageMagick
