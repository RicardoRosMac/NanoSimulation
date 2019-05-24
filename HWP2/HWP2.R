library(parallel)
library("sna")
library(ggplot2)

dim <- 30
num <- dim^2
limit <- 10
repeatexp <- 30
probability <- seq(from=0.1, to=0.9 ,by=0.10)

paso <- function(pos) {
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim), max(columna - 1, 1): min(columna + 1, dim)]
  return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

results <- data.frame()

for(p in probability){
  iterations <- list()
  actual <- matrix(1 * (runif(num) < p) , nrow=dim, ncol=dim)
  suppressMessages(library("sna"))
  for (rep in 1:repeatexp){
    if(sum(actual) == 0){ #todos se generaron muertos
      iterations <- c(iterations, 0)
      iterations <- unlist(iterations)
    } else if(sum(actual) == num){ #todos estan vivos
      iterations <- c(iterations , 0)
      iterations <- unlist(iterations)
    } else{
      png(paste("p2_Rep",rep, "_t0.png", sep =""))
      plot.sociomatrix(actual, diaglab = FALSE, main = "Inicio")
      graphics.off()
      for(iteracion in 1:limit) {
        clusterExport(cluster, "actual")
        siguiente <- parSapply(cluster, 1:num, paso)
        if(sum(siguiente) == 0) { # todos murieron
          #print("Ya no queda nadie vivo.")
          break;
        }
        actual <- matrix(siguiente, nrow=dim, ncol = dim, byrow = TRUE)
        salida <- paste("p2_Rep",rep, "_t", iteracion, ".png", sep="")
        tiempo <- paste("Paso", iteracion)
        png(salida)
        plot.sociomatrix(actual, diaglab = FALSE, main = tiempo)
        graphics.off()
      }
      iterations <- c(iterations, iteracion)
      iterations <- unlist(iterations)
    }
  }
  results <- rbind(results, iterations)
}
stopCluster(cluster)

colnames(results) <- as.character(1:repeatexp) 
results <- cbind("Prob" = probability, results)
results$probability <- as.factor(results$probability)
results <- melt(results, id.vars = "Prob")


ggplot(data=results, aes(x=Prob, y=results$value)) + 
  geom_bar(stat="identity", position="dodge", fill = "#FF6666") +
  theme_gray(base_size = 14) + xlab("Probabilidad") + ylab("iterations") + 
  geom_text(aes(label = Prob), position= position_dodge(width= 0.1), vjust = -0.5) +
  ggsave("Probaiter.png")
graphics.off()

system("convert -delay 50 -size 300x300 p2_Rep1_t*.png -loop 0 AC.gif") # creamos animacion con ImageMagick

