###Código paralelizado###

library(parallel)

PSprocess <- Sys.time()
repetir <- 30 
dur <-200 
dimen <- 1:8

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dur")
datos <-  data.frame()

for (dim in dimen) {
  clusterExport(cluster, "dim")
  resultado <- parSapply(cluster, 1:repetir,
                         function(x) {
                           pos <- rep(0, dim)
                           origen <- rep(0, dim)
                           contador <- 0
                           for (t in 1:dur) {
                             cambiar <- sample(1:dim, 1)
                             cambio <- 1
                             if (runif(1) < 0.5) {
                               cambio <- -1
                             }
                             pos[cambiar] <- pos[cambiar] + cambio
                             d <- dist(pos)
                             if (sum(pos == origen) == dim) {
                               contador <-  contador + 1
                             }
                           }
                           return(contador)
                         }
  )
  datos <- rbind(datos, resultado)
}
stopCluster(cluster)

PEprocess <- Sys.time()
TimeP <- PEprocess - PSprocess
print(TimeP)

###Código sin paralelizar###

Sprocess <- Sys.time()
datos <-  data.frame()
for (dim in dimen) {
  resultado <- sapply(1:repetir,
                      function(x) {
                        pos <- rep(0, dim)
                        origen <- rep(0, dim)
                        contador <- 0
                        for (t in 1:dur) {
                          cambiar <- sample(1:dim, 1)
                          cambio <- 1
                          if (runif(1) < 0.5) {
                            cambio <- -1
                          }
                          pos[cambiar] <- pos[cambiar] + cambio
                          d <- dist(pos)
                          if (sum(pos == origen) == dim) {
                            contador <-  contador + 1
                          }
                        }
                        return(contador)
                      }
  )
  datos <- rbind(datos, resultado)
}

Eprocess <- Sys.time()
TimeSP <- Eprocess - Sprocess
print(TimeSP)

####Comparación###
vstime <- data.frame(TimeSP, TimeP)
print(vstime)
ftc <- kruskal.test(vstime)
ftc$p.value
ftc$p.value < 0.05
