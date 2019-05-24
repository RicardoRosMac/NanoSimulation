rep <- 1
dur <- 100
dim <- 8
orig <- rep(0, dim)
tab <- numeric(length(rep*dim))
elapsed <- numeric(length(dur))
i = 1
j = 1
anterior <- 0
timeT <- system.time(
  for (dimension in dim:dim) {
    timeDim <- system.time(
      for (repeticion in 1:rep)  { 
        cero <- 0              
        pos <- rep(0, dimension)
        for (t in 1:dur) {
          timeD <- system.time(
            for (x in 1:1) {
              cambiar <- sample(1:dimension, 1)                     
              if (runif(1) < 0.5) {
                cambio <- 1
              } else {
                cambio <- -1
              }
              pos[cambiar] <- pos[cambiar] + cambio
              if (all (pos == orig)) {
                cero = cero + 1
              }
            })[3]
          time <- (timeD + anterior)
          elapsed[j] <- time
          anterior <- time
          j = j + 1
        }
        prob <- ((cero/dur)*100)
        tab[i] <- prob
        i = i + 1
      })[3]
  })
mat <- matrix (tab, ncol=dim, nrow=rep)
png("tiempo.png")
plot (elapsed, type="l", xlab="N\u{FA}mero de pasos", ylab="Tiempo (s)")
graphics.off()
