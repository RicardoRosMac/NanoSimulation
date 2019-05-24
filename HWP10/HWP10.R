library(testit)
library(parallel)
library(ggplot2)

#############Funciones
knapsack <- function(cap, peso, valor) {
  inicio <- as.numeric(Sys.time())
  n <- length(peso)
  pt <- sum(peso) # deben ser enteros en este caso
  
  assert(n == length(valor))
  
  vt <- sum(valor) # pueden ser lo que sea
  
  if (pt < cap) { # cabe todo
    final <- as.numeric(Sys.time())
    return(c(final - inicio, vt))
  } else {
    filas <- cap + 1 # una para cada posible peso acumulado desde cero hasta cap
    cols <- n + 1 # una para cada objeto y una extra al inicio para no llevar nada
    
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) # al inicio todo vale negativo infinito
    
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 # todas las filas tienen un cero al inicio (no llevar nada da cero valor)
    }
    
    rownames(tabla) <- 0:cap # filas corresponden a pesos acumulados posibles
    colnames(tabla) <- c(0, valor) # columnas corresponden a objetos considerados
    
    for (objeto in 1:n) { # consideremos a cada objeto por turno
      p <- peso[objeto] # tomamos su peso a una variable
      v <- valor[objeto] # y su valor a otra variable
      
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - p
        tabla[acum, objeto + 1] <- tabla[acum, objeto]
        
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + v)
        } 
      }
      
    }
    final <- as.numeric(Sys.time())
    return(c(final - inicio, max(tabla)))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1, media, desv))
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

###########CódigoSecuencial

#Cantidad de objetos
n <- 50

#Generadores
pesos <- generador.pesos(n, 15, 80)
valores <- generador.valores(pesos, 10, 500)
capacidad <- round(sum(pesos) * 0.65)

#Optimo
optimo <- knapsack(capacidad, pesos, valores)

#Tamaño de la poblacion
init <- 200

p <- poblacion.inicial(n, init)
tam <- dim(p)[1]

assert(tam == init)

pm <- 0.05 #probabilidad de mutacion
rep <- 50 #cantidad de reproducciones
tmax <- 50

mejores <- double()

for (iter in 1:tmax) {
  p$obj <- NULL
  p$fact <- NULL
  
  for (i in 1:tam) { # cada individuo puede mutarse con probabilidad pm
    if (runif(1) < pm) {
      p <- rbind(p, mutacion(p[i,], n))
    }
  }
  
  for (i in 1:rep) { # una cantidad fija de reproducciones
    padres <- sample(1:tam, 2, replace = FALSE)
    hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
    p <- rbind(p, hijos[1:n]) # primer hijo
    p <- rbind(p, hijos[(n+1):(2*n)]) # segundo hijo
  }
  
  tam <- dim(p)[1]
  obj <- double()
  fact <- integer()
  
  for (i in 1:tam) {
    obj <- c(obj, objetivo(p[i,], valores))
    fact <- c(fact, factible(p[i,], pesos, capacidad))
  }
  
  p <- cbind(p, obj)
  p <- cbind(p, fact)
  mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
  p <- p[mantener,]
  tam <- dim(p)[1]
  
  assert(tam == init)
  
  factibles <- p[p$fact == TRUE,]
  mejor <- max(factibles$obj)
  mejores <- c(mejores, mejor)
}

############CódParalelo

startP=Sys.time()
n <- 50        #Cantidad de objetos
generaciones <- c(200, 250, 300)     #Tamaño de la poblacion
pmutaciones <- c(0.05, 0.1, 0.2)      #probabilidad de mutacion
reproducciones <- c(50, 75, 100)       #cantidad de reproducciones
iteraciones <- c(10, 15, 20)      #Tiempo maximo
replicas <- 5  #Replicas

#Data Frame
datos <- data.frame(Réplica = integer(), Optimo = integer(),
                    Generaciones = numeric(), ProbMut = numeric(),
                    Reproducciones = numeric(), Iteraciones = numeric(),
                    Objetivo = double())

#Creamos cluster
cluster <- makeCluster(detectCores())

#Generadores
pesos <- generador.pesos(n, 15, 80)
valores <- generador.valores(pesos, 10, 500)
capacidad <- round(sum(pesos) * 0.65)

#Optimo
opt <- knapsack(capacidad, pesos, valores)[2]

clusterExport(cluster, "n")
clusterExport(cluster, "pesos")
clusterExport(cluster, "valores")
clusterExport(cluster, "capacidad")

for (tmax in iteraciones) {
  for (rep in reproducciones) {
    for (pm in pmutaciones) {
      for (init in generaciones) {
        for (r in 1:replicas) {
          
          #Inicio del genetico parsapply
          pobl <- t(parSapply(cluster, 1:init, function(i) {
            return(round(runif(n)))
          }))
          
          p <- as.data.frame(pobl)
          tam <- dim(p)[1]
          
          assert(tam == init)
          
          mejores <- double()
          
          for (iter in 1:tmax) {
            p$obj <- NULL
            p$fact <- NULL
            
            clusterExport(cluster, "p")
            clusterExport(cluster, "tam")
            
            probabilidades <- runif(tam) < pm
            mutados <- which(probabilidades %in% TRUE)
            
            mutaciones <- t(parSapply(cluster, mutados, function(i) {
              pos <- sample(1:n, 1)
              mut <- p[i,]
              mut[pos] <- (!p[i,][pos]) * 1
              return(as.numeric(mut))
            }))
            
            hijos <- matrix(parSapply(cluster, 1:rep, function(i) {
              padres <- sample(1:tam, 2, replace = FALSE)
              pos <- sample(2:(n-1), 1)
              x <- p[padres[1],]
              y <- p[padres[2],]
              xy <- c(x[1:pos], y[(pos+1):n])
              yx <- c(y[1:pos], x[(pos+1):n])
              return(as.numeric(c(xy, yx)))
            }), ncol = n, byrow = TRUE)
            
            p <- rbind(p, mutaciones, hijos)
            tam <- dim(p)[1]
            
            clusterExport(cluster, "p")
            clusterExport(cluster, "tam")
            
            p$obj <- parSapply(cluster, 1:tam, function(i) {
              return(sum(p[i,] * valores))
            })
            
            p$fact <- parSapply(cluster, 1:tam, function(i) {
              return(sum(p[i,] * pesos) <= capacidad)
            })
            
            mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
            
            p <- p[mantener,]
            tam <- dim(p)[1]
            
            assert(tam == init)
            
            factibles <- p[p$fact == TRUE,]
            mejor <- max(factibles$obj)
            mejores <- c(mejores, mejor)
            
          }
          
          datos <- rbind(datos, data.frame(Réplica = r, Optimo = optimo,
                                           Generaciones = init, ProbMut = pm,
                                           Reproducciones = rep, Iteraciones = tmax,
                                           Objetivo = max(mejores)))
        }
      }
    }
  }
}
stopCluster(cluster)
endP=Sys.time()
