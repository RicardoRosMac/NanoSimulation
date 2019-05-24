modelos <- read.csv("~/Documents/Maestría/Simulation R HW/HWP12/digitosext.modelo.csv", sep=" ", header=FALSE, stringsAsFactors=F)
modelos[modelos=='n'] <- 0.995 
modelos[modelos=='g'] <- 0.92 
modelos[modelos=='b'] <- 0.002

r <- 7
c <- 5
dim <- r * c

n <- 49
w <- ceiling(sqrt(n))
h <- ceiling(n / w)

SASCIIAD <- c(0:9,"E", "F", "U", "L", "I", "C", "H", "T", "P", "A", "W", "M")

png("plantilla.png", width=1600, height=2000)
par(mfrow=c(w, h), mar = c(0,0,7,0))
suppressMessages(library("sna"))

for (j in 1:n) {
  d <- sample(0:21, 1)
  pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
  imagen <- matrix(pixeles, nrow=r, ncol=c, byrow=TRUE)
  plot.sociomatrix(imagen, drawlab=FALSE, diaglab=FALSE, 
                   main=paste(SASCIIAD[d+1], ""), cex.main=5)
}
graphics.off()
