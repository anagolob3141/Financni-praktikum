library(actuar)

#Legenda slučajnih spremenljivk:
# Y...višina škodnega zahtevka
# S...komulativna škoda
# N...število odškodninskih zahtevkov

# 1. Naloga:

# (a) Uvoz vektorja vzorcev in izris histohrama odškodnin:

vzorec <- scan("vzorec2.txt")
graf1 <- hist(vzorec, col="cadetblue3", xlab="Višina odškodnine", ylab="Frequency", main="Histogram odškodnin")


# (b) Izračun neznanih parametrov Weibull porazdelitve:

parametri <- mde(vzorec, pweibull,start=list(shape= 3, scale= 2))
shape <- parametri$estimate[1]
scale <- parametri$estimate[2]

# (c) Histogram z dodano gostoto spremenljivke Y:

graf2 <- hist(vzorec, col="cadetblue3", xlab="Višina odškodnine", ylab="Density", main="Histogram odškodnin",prob=TRUE)   
curve(dweibull(x, shape, scale),ylim=c(0,0.20), col="brown",lwd=2,add=TRUE)
legend("topright", legend=c("Weibull porazdelitev"),
       col=c("brown"), lty=1:2, cex=0.8)

# Primerjava vzorčne in porazdelitvene funkcije:

graf3 <- plot(stepfun(sort(vzorec),seq(0,1,(1/length(vzorec)))), main = "Porazdelitvena funkcija odškodnin",
              xlab = "Višina odškkodnin", ylab = "Porazdelitvena funkcija", col="cadetblue3", pch=20)

graf3 <- curve(pweibull(x, shape, scale), xlim=c(0,10), col="red",add=TRUE)

legend("bottomright", legend = c("empirična porazdelitev", "weibull porazdelitev"), 
       col= c("cadetblue3", "red"), lwd=1:2, cex=0.8)

# (d)
# Pričakovana vrednost in varianca weibullove porazdelitve (Y):

exY <- scale * gamma(1 + 1/shape)
varY <- (scale ** 2) * (gamma(1 + 2/shape) - (gamma(1 + 1/shape) ** 2))

# Pričakovana vrednost normalne porazdelitve (N):

n = 20
p = (3/4)
exN <- n * p
varN <- n * p * (1 - p)

# Pričakovana vrednost odškodninskih zahtevkov z Waldovima identitetama (S):

upanjeS1 <- exN * exY # =54.98702
variancaS1 <- varY * exN + (exY **2) * varN
disperzijaS1 <- (variancaS1) ** (1/2) # =10.51552

# 2. Naloga:------------------------------------------------------------------------------------
# (a) d1...Diskretizacija porazdelitve s. S. Y

h <- 0.25
d1 <- discretize(pweibull(x, shape, scale), 0, 50*h, step = h, method = "round")

# (b) Graf porazdelitve s. s. Y in porazdelivena funkcija njene diskretizacije:

x <- seq(0, (49 * h), h)

graf4 <- plot(stepfun(x, diffinv(d1)), pch=".", col = "cadetblue3",lwd=3,
              main = "Weibull porazdelitev", xlab = "x", ylab = "Porazdelitvena funkcija")
graf4 <- curve(pweibull(x, shape, scale), xlim=c(0,10), col="red",add=TRUE)

# (c) pnS... Porazdelitvena funkcija komulativne škode s Panjerjevim algoritmom:

pnS <- aggregateDist(method = "recursive", model.sev = d1,
                     model.freq = "binomial", size = n, prob = p, x.scale = 0.25)

# (d) Disperzija in upanje komulativne škode (S):

x <- knots(pnS) # kje so skoki
skoki <- diff(pnS) # kako veliki so skoki

upanjeS2 <- sum(x * skoki) # = 54.6826

kvadratUpanja <- sum((x ** 2) * skoki)
variancaS2 <- kvadratUpanja - (upanjeS2 ** 2)

disperzijaS2 <- variancaS2 ** (1/2) # =11.1198

print(c(upanjeS2, disperzijaS2))

# (e)
# tv1- tvegana vrednost pri 99.5%
# pi1- pričakovan izpad pri 0.5%
tv1 <- VaR(pnS, 0.995) # =92.500
pi1 <- CTE(pnS, 0.005) # =55.091

print(c(tv1, pi1))


# 3. Naloga: ----------------------------------------------------------------------------------------------

# (a) 
#stimS...Monte Carlo stimulacija s. s. S:


monteCarloStim <- function(){
  N <- rbinom(1000, n, p)
  stimS <- numeric(1000)
  
  for (i in 1:1000){
    stimS[i] <- sum(rweibull(N[i], shape, scale))
  }
  return(stimS)
}

stimS <- monteCarloStim()

# (b) Ocena upanja in disperzije:

upanjeS3 <- mean(stimS) # =55.13992
variancaS3 <- var(stimS)
disperzijaS3 <- variancaS3 ** (1/2) # =10.67256

# (c) 
# tv2- tvegana vrednost pri 99.5%

s <- stepfun(seq(0,(1-(2/length(stimS))),(1/length(stimS))),sort(stimS))
tv2 <- s(0.995) # =87.36739

#Ocena tvegane vrednosti dobljena s Panjerjevim algoritmom je 92.5 in je nekoliko višja.  

# (d) Graf simulirane porazdelitvene funkcije:

graf5 <- plot(pnS, xlim = c(0,100), col="cadetblue3")
graf5 <- plot(stepfun(sort(stimS),seq(0,1,(1/length(stimS)))),col="red", add = TRUE)
legend("bottomright", legend = c("Panjerjev algoritem", "Monte Carlo stimulacija"), 
       col= c("cadetblue3", "red"), lwd=2:1, cex=0.9)

