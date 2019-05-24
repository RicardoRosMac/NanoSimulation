library(ggplot2)
library(distr)
library(parallel)

inicio <- -6
final <- -inicio
paso <- 0.25
x <- seq(inicio, final, paso)
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
png("p5f.png") # dibujamos f(x) para ver como es
plot(x, (2/pi) * (1/(exp(x)+exp(-x))))
lines(x, (2/pi) * (1/(exp(x)+exp(-x))), type="l")
graphics.off()
suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador <- r(AbscontDistribution(d = g)) # creamos un generador
muestra <- generador(50000) # sacamos una muestra
png("p5m.png") # validamos con un dibujo
hist(muestra, freq=F, breaks=50,
     main="Histograma de g(x) comparado con g(x)",
     xlim=c(inicio, final), ylim=c(0, 0.4))
lines(x, g(x), col="red") # dibujamos g(x) encima del histograma
graphics.off()


desde <- 3
hasta <- 7
Pedazos <- 300
cuantos <- 100
muestra <- c(50, 100, 500, 1000, 5000, 10000) #Tamaño de muestra
wolfram<-0.048834 # Valor real
repeticiones <- 12
parte <- function() {
  val <- generador(pedazo)
  return(sum(val >= desde & val <= hasta))
}
Info <- data.frame()
for (pedazo in Pedazos) {
  for (cuantos in muestra){
    for (vez in 1:repeticiones){
      suppressMessages(library(doParallel))
      clust <- makeCluster(2)
      registerDoParallel(clust)
      montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
      stopCluster(clust)
      integral <- sum(montecarlo) / (cuantos * pedazo)
      piInt <- (pi / 2) * integral
      pI <- c(piInt, cuantos, as.integer(pedazo))
      print(cuantos)
      Info <- rbind(Info, pI)
    }
  }
}
colnames(Info) <- c("val", "rep", "esp")
Info$esp <- as.integer(Info$esp)
Info$rep <- as.factor(Info$rep)
print(Info)

agregando <- function(Espacio){
  Espacio <- as.factor(Espacio)
  return(paste("n = ", Espacio, sep = ""))
}
valmin <- (wolfram-50)

palette(rainbow(length(levels(Info$rep))))
ggplot(Info, aes(x = rep, y = val, fill = rep)) +
  geom_violin(trim = TRUE) + theme_minimal(base_size = 14) + labs(x = "Tamaño de muestra", y = "valores")  +
  theme(legend.position="none") + facet_grid(.~esp, labeller = as_labeller(agregando)) +
  geom_hline(yintercept = wolfram, size=1.5, color = "red") + stat_summary(fun.y=median, geom="point", size=2, color="blue") +
  scale_y_continuous(breaks = c(seq(min(Info$val), max(Info$val), length.out =7), valmin, wolfram), 
                     labels =  c(formatC(seq(min(Info$val), max(Info$val), length.out =7), 6, format = "f"), "0.048800", wolfram)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) + stat_summary(fun.y=median, geom="line", aes(group=1)) +
  geom_hline(yintercept = 0.048830) + geom_hline(yintercept = 0.048800)
