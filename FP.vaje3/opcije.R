# 3. Naloga za finančni praktikum:

# S0 = trenutna cena delnice (t=0)
# u = parameter binomskega modela
# d = parameter binomskega modela
# t = zapadlost opcije in število obdobij v modelu
# r = obdobna obrestna mera
# w = zaporedje uteži
# type = tip opcije: nakupna/prodajna
# N = število simulacij pri Monte Carlo vrednotenju
# vrsta = pot cene delnice

library(combinat)
S0 <- 50
u <- 1.05
d <- 0.95
t <- 5
r <- 0.03
w <- c(1, 2, 3, 4, 5, 6)

# 1. Naloga: 
# (a):
vrstica1 <- c(50, 52.5, 49.88, 47.38, 45.01, 47.26)
vrstica2 <- c(50, 52.5, 55.12, 57.88, 60.78, 63.81)
vrstica3 <- c(50, 47.5, 49.88, 47.38, 45.01, 42.76)
vrstica4 <- c(50.00, 47.50, 45.12, 47.38, 45.01, 47.26)
vrstica5 <- c(50.00, 52.50, 49.88, 52.37, 54.99, 52.24)

k1 <- (sum(vrstica1 * w)/sum(w))
k2 <- (sum(vrstica2 * w)/sum(w))
k3 <- (sum(vrstica3 * w)/sum(w))
k4 <- (sum(vrstica4 * w)/sum(w))
k5 <- (sum(vrstica5 * w)/sum(w))

izplaciloX1 <- max(0, vrstica1[6] - k1)
izplaciloX2 <- max(0, vrstica2[6] - k2)
izplaciloX3 <- max(0, vrstica3[6] - k3)
izplaciloX4 <- max(0, vrstica4[6] - k4)
izplaciloX5 <- max(0, vrstica5[6] - k5)

izplaciloY1 <- -min(0, vrstica1[6] - k1)
izplaciloY2 <- -min(0, vrstica2[6] - k2)
izplaciloY3 <- -min(0, vrstica3[6] - k3)
izplaciloY4 <- -min(0, vrstica4[6] - k4)
izplaciloY5 <- -min(0, vrstica5[6] - k5)

nakupna <- c(izplaciloX1, izplaciloX2, izplaciloX3, izplaciloX4, izplaciloX5)
prodajna <- c(izplaciloY1,izplaciloY2, izplaciloY3, izplaciloY4, izplaciloY5)
tabela1 <- rbind(nakupna, prodajna)
colnames(tabela1) <- c('Pot 1', 'Pot 2', 'Pot 3', 'Pot 4', 'Pot 5')

# Spodnja tabela prikazuje izplačila prodajne in nakupne opcije za 5 možnih poti cene delnice.
head(tabela1)

# (b):Funkcija, ki določa izplačilo cene ob zapadlosti.

izplacilo <- function(vrsta, w, type){
  k <- (sum(vrsta * w)/sum(w))
  if (type == "call"){
    izpl <- max(0, vrsta[length(vrsta)] - k)
  } else {
    izpl <- -min(0, vrsta[length(vrsta)] - k)
  }
  return(izpl)
}

# Naloga 2:
# (a):Funkcija, ki določi premijo opcije z analizo polnega binomskega drevesa.

binomski <- function(s0=50,u=1.05,d=0.95,r=0.03,t=5,w,type){
  q = (1 + r - d)/(u - d)
  poti <- hcube(rep(2,t),1,-1)
  verjetnosti <- q ** rowSums(poti) * (1 - q) ** (t -rowSums(poti))
  razvoji <- u ** poti * d ** (1 - poti)
  S0 <- rep(s0, nrow(razvoji))
  cene <- cbind(S0, razvoji)
  matrika <- t(apply(cene, 1, cumprod))
  vektorIzplacil <- apply(matrika, 1, izplacilo, w=w, type = type)
  premija <- sum(vektorIzplacil * verjetnosti)/(1+r)**t
  return(premija)
}

# (b):Funkcija, ki oceni premijo opcije z metodo Monte Carlo.

monte <- function(s0,u,d,r,t,w,type,N){
  q = (1 + r - d)/(u - d)
  izplacila <- rep(0,N)
  for(i in 1:N){
    poti <- rbinom(t, 1, q)
    cene <- rep(0,t + 1)
    cene[1] <- s0
    for(j in 2:(t+1)){
      cene[j] <- cene[j-1] * (u ** poti[j-1]) * (d ** (1 - poti[j-1]))
    }
    izplacila[i] <- izplacilo(cene, w=w, type=type)
  }
  premija <- sum(izplacila)/ (N * (1 + r) ** t)
  return(premija)
}

N1 = 10
N2 = 100
N3 = 1000

premijaN1 <- monte(60, 1.05, 0.95, 0.01, 15, rep(1, 16), type= 'put', N1)
premijaN2 <- monte(60, 1.05, 0.95, 0.01, 15, rep(1, 16), type= 'put', N2)
premijaN3 <- monte(60, 1.05, 0.95, 0.01, 15, rep(1, 16), type= 'put', N3)

# 3.Naloga:

#(a) Ocenjevanje natančnosti metode Monte Carlo:

M = 100
ocenjevanje <- function(M, N){
  ocene <- rep(0, M)
  for(i in 1:M){
    ocene[i] <- monte(60, 1.05, 0.95, 0.01, 15, rep(1, 16), type= 'put', N)
  }
  return(ocene)
}

M = 100
oceneN1 <- ocenjevanje(M, N1)
oceneN2 <- ocenjevanje(M, N2)
oceneN3 <- ocenjevanje(M, N3)


#(b) Povprečna ocena in odklon za N1, N2, N3:

povprečjeN1 <- mean(oceneN1)
povprečjeN2 <- mean(oceneN2)
povprečjeN3 <- mean(oceneN3)

odklonN1 <- (sum((oceneN1 - povprečjeN1) ** 2) / length(oceneN1)) ** (1/2)
odklonN2 <- (sum((oceneN2 - povprečjeN2) ** 2) / length(oceneN2)) ** (1/2)
odklonN3 <- (sum((oceneN3 - povprečjeN3) ** 2) / length(oceneN3)) ** (1/2)

binomska <- binomski(60, 1.05, 0.95, 0.01, 15, rep(1, 16), type= 'put')

# Histogrami vzorčnih porazdelitev ocen premije:

# Za N=10:
grafN1 <- hist(oceneN1, col="cadetblue3", xlim = c(0,5), xlab="Premija", ylab="Frequency", main="Monte Carlo, N = 10")
grafN1 <- abline(v=povprečjeN1, col='green',lwd=2.7)
grafN1 <- abline(v=binomska, col='red',lty=2, lwd=2.7)
grafN1 <- arrows(povprečjeN1,0, povprečjeN1 + odklonN1, 0,col='green',length = 0.10, angle = 25)
grafN1 <- arrows(povprečjeN1,0, povprečjeN1 - odklonN1, 0,col='green',length = 0.10, angle = 25)
legend("topright", legend=c("Monte Carlo", "analiza modela"),
       col=c("green", "red"), lty=1:2, cex=0.8)


# Za N=100:
grafN2 <- hist(oceneN2, col="cadetblue3", xlim = c(0,5), xlab="Premija", ylab="Frequency", main="Monte Carlo, N = 100")
grafN2 <- abline(v=povprečjeN2, col='green', lwd=2.7)
grafN2 <- abline(v=binomska, col='red',lty=2, lwd=2.7)
grafN2 <- arrows(povprečjeN2,0, povprečjeN2 + odklonN2, 0,col='green',length = 0.10, angle = 25)
grafN2 <- arrows(povprečjeN2,0, povprečjeN2 - odklonN2, 0,col='green',length = 0.10, angle = 25)
legend("topright", legend=c("Monte Carlo", "analiza modela"),
       col=c("green", "red"), lty=1:2, cex=0.8)

# Za N=1000:
grafN3 <- hist(oceneN3, col="cadetblue3", xlim = c(0,5), xlab="Premija", ylab="Frequency", main="Monte Carlo, N = 1000")
grafN3 <- abline(v=povprečjeN3, col='green', lwd=2.7)
grafN3 <- abline(v=binomska, col='red',lty=2, lwd=2.7)
grafN3 <- arrows(povprečjeN3,0, povprečjeN3 + odklonN3, 0,col='green',length = 0.10, angle = 25)
grafN3 <- arrows(povprečjeN3,0, povprečjeN3 - odklonN3, 0,col='green',length = 0.10, angle = 25)
legend("topright", legend=c("Monte Carlo", "analiza modela"),
       col=c("green", "red"), lty=1:2, cex=0.8)


