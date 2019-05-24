#Distancia
euclideana <- function(p1, p2) {
  return(sqrt(sum((p1 - p2)**2)))
}

manhattan <- function(p1, p2) {
  return(sum(abs(p1 - p2)))
}

ed.orig <- function(p) {
  dimension <- length(p)
  origen <- rep(0, dimension)
  return(euclideana(p, origen))
}

md.orig <- function(p) {
  dimension <- length(p)
  origen <- rep(0, dimension)
  return(manhattan(p, origen))
}


#Práctica

library(parallel)
library(reshape2)
library(RColorBrewer)
library(ggplot2)

rep <- 30
pasos <- sapply(6:12, function(x) {2**x})
eucl <- FALSE

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dur")
clusterExport(cluster, "eucl")
clusterExport(cluster, "pasos")
datos <- data.frame()
for (dur in pasos) {
  for (dimension in 1:8) {
    counter <- 0
    clusterExport(cluster, "dimension")
    resultado <- parSapply(cluster, 1:rep,
                           function(r) {
                             pos <- rep(0, dimension)
                             test <- rep(0, dimension)
                             mayor <- 0
                             counter <- 0
                             for (t in 1:dur) {
                               cambiar <- sample(1:dimension, 1)
                               cambio <- 1
                               if (runif(1) < 0.5) {
                                 cambio <- -1
                               }
                               pos[cambiar] <- pos[cambiar] + cambio
                               if (eucl) {
                                 d <- sum(sqrt(pos**2))
                               } else { # Manhattan
                                 d <- sum(abs(pos))
                               }
                               if (d > mayor) {
                                 mayor <- d
                               }
                               if (all(pos==test)){
                                 counter <- counter + 1
                               } 
                             }
                             return(counter)
                           })
    datos <- rbind(datos, resultado)
  }
}

stopCluster(cluster)

Adata <- data.frame(datos) 
nom <- as.character(rep(1:rep))
colnames(Adata) <- nom
Adata["Dimensi\u{F3}n"] <- as.character(rep(1:8, 7))
Adata["Duraci\u{F3}n"] <- rep(pasos, each=8)
Adata["Log"] <- log2(Adata$Duración)
Adata[nom] <- {Adata[nom]/Adata$Duración}
Adata <- melt(Adata, id.vars = c("Duraci\u{F3}n", "Dimensi\u{F3}n", "Log"), measure.vars = nom)
Adata["variable"] <- NULL
Adata$value <- Adata$value + 0.0000000001
plotty <- ggplot(Adata) + aes(x = Dimensión, y = log(value), fill = Dimensión) + geom_boxplot() + 
  facet_grid(~Log, scale = "fixed", labeller = label_bquote(cols = 2 ^.(Log)) )
plotty <- plotty + xlab("Dimensiones") + ylab("Probabilidad de regreso al origen (log)")
if (eucl) {
  plotty <- plotty + ggtitle("Incrementos")
  plotty <- plotty + scale_fill_brewer(palette="Paired")+ theme_minimal()+ theme(plot.title = element_text(hjust = 0.5))
  ggsave("d_Eucl.eps")
} else {
  plotty <- plotty + ggtitle("Incrementos")
  plotty <- plotty + scale_fill_brewer(palette="Paired")+ theme_minimal() + theme(plot.title = element_text(hjust = 0.4))
  ggsave("d_Manh.eps")
}