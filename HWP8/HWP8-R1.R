library(testit) 
library(ggplot2)
library(parallel) 

k <- c(100, 1000, 10000)
n <- c(10000, 100000, 1000000)
originales <- rnorm(k) 
cumulos <- originales - min(originales) + 1 
cumulos <- round(n * cumulos / sum(cumulos)) 
assert(min(cumulos) > 0) 
diferencia <- n - sum(cumulos) 
if (diferencia > 0) { 
  for (i in 1:diferencia) { 
    p <- sample(1:k, 1) 
    cumulos[p] <- cumulos[p] + 1 
  } 
} else if (diferencia < 0) { 
  for (i in 1:-diferencia) { 
    p <- sample(1:k, 1) 
    if (cumulos[p] > 1) { 
      cumulos[p] <- cumulos[p] - 1 
    } 
  } 
} 
assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios 
assert(sum(cumulos) == n) 
c <- median(cumulos) # tamaño critico de cumulos 
d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva 
rotura <- function(x) { 
  return (1 / (1 + exp((c - x) / d))) 
} 
union <- function(x) { 
  return (exp(-x / c)) 
} 
romperse <- function(tam, cuantos) { 
  romper <- round(rotura(tam) * cuantos) # independientes 
  resultado <- rep(tam, cuantos - romper) # los demas 
  if (romper > 0) { 
    for (cumulo in 1:romper) { # agregar las rotas 
      t <- 1 
      if (tam > 2) { # sample no jala con un solo valor 
        t <- sample(1:(tam-1), 1) 
      } 
      resultado <- c(resultado, t, tam - t) 
    } 
  } 
  assert(sum(resultado) == tam * cuantos) # no hubo perdidas 
  return(resultado) 
} 
unirse <- function(tam, cuantos) { 
  unir <- round(union(tam) * cuantos) # independientes 
  if (unir > 0) { 
    division <- c(rep(-tam, unir), rep(tam, cuantos - unir)) 
    assert(sum(abs(division)) == tam * cuantos) 
    return(division) 
  } else { 
    return(rep(tam, cuantos)) 
  } 
} 


freq <- as.data.frame(table(cumulos)) 
names(freq) <- c("tam", "num") 
freq$tam <- as.numeric(levels(freq$tam))[freq$tam] 
duracion <- 5
digitos <- floor(log(duracion, 10)) + 1 

funromperse <- function(i){ 
  return(as.vector(romperse(as.numeric(freq[i,][1]), as.numeric(freq[i,][2])))) 
} 
fununirse <- function(i){ 
  return(unirse(as.numeric(freq[i,][1]), as.numeric(freq[i,][2]))) 
} 

cluster <- makeCluster(detectCores()-1) 
clusterExport(cluster, "romperse") 
clusterExport(cluster, "rotura") 
clusterExport(cluster, "funromperse") 
clusterExport(cluster, "c") 
clusterExport(cluster, "d") 
clusterExport(cluster, "assert") 
clusterExport(cluster, "fununirse") 
clusterExport(cluster, "union") 
clusterExport(cluster, "unirse") 

vartime <- data.frame()
for (repeticiones in 1:30) {
  tiempoinicial <- Sys.time()
  for (paso in 1:duracion) { 
    assert(sum(cumulos) == n) 
    cumulos <- integer() 
    clusterExport(cluster, "freq") 
    cumulos <- as.vector(parSapply(cluster, 1:(dim(freq)[1]), funromperse)) 
    a <- c() 
    for(i in 1:length(cumulos)){ 
      a <-c(a, cumulos[[i]]) 
    } 
    cumulos <- a 
    assert(sum(cumulos) == n) 
    assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios 
    freq <- as.data.frame(table(cumulos)) # actualizar urnas 
    names(freq) <- c("tam", "num") 
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam] 
    assert(sum(freq$num * freq$tam) == n) 
    cumulos <- integer() 
    clusterExport(cluster, "freq") 
    cumulos <- as.vector(parSapply(cluster, 1:(dim(freq)[1]), fununirse)) 
    a <- c() 
    for(i in 1:length(cumulos)){ 
      a <-c(a, cumulos[[i]]) 
    } 
    cumulos <- a 
    assert(sum(abs(cumulos)) == n) 
    assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios 
    juntarse <- -cumulos[cumulos < 0] 
    cumulos <- cumulos[cumulos > 0] 
    assert(sum(cumulos) + sum(juntarse) == n) 
    nt <- length(juntarse) 
    if (nt > 0) { 
      if (nt > 1) { 
        juntarse <- sample(juntarse) 
        for (i in 1:floor(nt / 2) ) { 
          cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i]) 
        } 
      } 
      if (nt %% 2 == 1) { 
        cumulos <- c(cumulos, juntarse[nt]) 
      } 
    } 
    assert(sum(cumulos) == n) 
    freq <- as.data.frame(table(cumulos)) 
    names(freq) <- c("tam", "num") 
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam] 
    assert(sum(freq$num * freq$tam) == n) 
    tl <- paste(paso, "", sep="") 
    #while (nchar(tl) < digitos) { 
    tl <- paste("0", tl, sep="") 
    #} 
    #png(paste("p8_ctpar", tl, ".png", sep=""), width=300, height=300) 
    #tope <- 50 * ceiling(max(cumulos) / 50) 
    #hist(cumulos, breaks=seq(0, tope, 50),  
    #main=paste("Paso", paso, "con paralelizamiento"), freq=FALSE, 
    #ylim=c(0, 0.05), xlab="Tama\u{00f1}o", ylab="Frecuencia relativa") 
    #graphics.off()
  }
  tiempofinal<- Sys.time()
  tiempo <- tiempofinal - tiempoinicial
  vartime <- rbind(vartime, c(repeticiones,tiempo,duracion,2))
}

stopCluster(cluster) 
names(vartime) <- c("repeticion","tiempo","duracion","tipo")
mediavartime <- mean(vartime$tiempo)

ggplot(data = vartime) +
  aes(y = tiempo, x = seq_along(tiempo)) +
  geom_line(color = '#cb181d') +
  labs(x = 'Repetición ', y = 'Tiempo (Segundos)') + theme_gray()
