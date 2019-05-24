library(ggplot2)

l <- 1.5
n <- 50 #Número de agentes
pi <- 0.05 #Probabilidad de infeccion
pr <- 0.02 #Probabilidad de recuperación
v <- l / 30 
r <- 0.1
tmax <- 100 

pV <- c(0, 1, 0.1) #Variacion de la probablidad de vacunados 
IMax <- c()
for(pv in pV) { #Incremento en la probabilidad de vacunados
  for(rep in 1:30) { #Repeticiones del experimento 
    agentes <- data.frame(x = double(), y = double(), dx = double(), dy = double(), estado  = character())
    for (i in 1:n) {
      e <- "S"
      if(runif(1) < pv) { #Vacunar 
        e <- "R"
      } else { #Si no estan vacunados
        if (runif(1) < pi) {
          e <- "I"
        }
      }
      levels(agentes$estado) <- c("S", "I", "R")
      agentes <- rbind(agentes, data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                                           dx = runif(1, -v, v), dy = runif(1, -v, v),
                                           estado = e))
    }
    epidemia <- integer()
    digitos <- floor(log(tmax, 10)) + 1
   for (tiempo in 1:tmax) {
      infectados <- dim(agentes[agentes$estado == "I",])[1]
      epidemia <- c(epidemia, infectados)
      if (infectados == 0) {
        break
      }
      contagios <- rep(FALSE, n)
      for (i in 1:n) { # posibles contagios
        a1 <- agentes[i, ]
        if (a1$estado == "I") { # desde los infectados
          for (j in 1:n) {
            if (!contagios[j]) { # aun sin contagio
              a2 <- agentes[j, ]
              if (a2$estado == "S") { # hacia los susceptibles
                dx <- a1$x - a2$x
                dy <- a1$y - a2$y
                d <- sqrt(dx^2 + dy^2)
                if (d < r) { # umbral
                  p <- (r - d) / r
                  if (runif(1) < p) {
                    contagios[j] <- TRUE
                  }
                }
              }
            }
          }
        }
      }
  for (i in 1:n) { # movimientos y actualizaciones
        a <- agentes[i, ]
        if (contagios[i]) {
          a$estado <- "I"
        } else if (a$estado == "I") { # ya estaba infectado
          if (runif(1) < pr) {
            a$estado <- "R" # recupera
          }
        }
        a$x <- a$x + a$dx
        a$y <- a$y + a$dy
        if (a$x > l) {
          a$x <- a$x - l
        }
        if (a$y > l) {
          a$y <- a$y - l
        }
        if (a$x < 0) {
          a$x <- a$x + l
        }
        if (a$y < 0) {
          a$y <- a$y + l
        }
        agentes[i, ] <- a
      }
    }
    IMax <- c(IMax, max(epidemia)) #Se guardan los infectados maximos por cada replica
  }
}
Vacu <- data.frame(Probvac = seq(pV, 30), Imaxi = (IMax/n)*100) #Porcentaje de infectados maximo

tiff("Avac.tiff", units="in", width=12, height=6.8, res=300, compression = 'lzw')
g <- ggplot(Vacu, aes(Probvac,Imaxi))
g + geom_boxplot(alpha = 0.3,aes(fill= Probvac)) + theme_gray() + labs(x= "Probabilidad de vacuna",y = "Porcentaje máximo de infectados", fill ="Probabilidad de vacuna" ) + scale_fill_brewer(palette="Paired")
graphics.off()
