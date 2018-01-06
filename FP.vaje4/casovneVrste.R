# 4. vaja pri predmetu Finančni praktikum:
# Ana Golob
library(ggplot2)
# 1. Naloga:

# (a) Podatki o dnevnih cenah zlata pridobljeni iz spletne strani: https://www.investing.com/commodities/gold
ceneZlata <- read.table("ceneZlata.txt")
head(ceneZlata)

# (b) Graf časovne vrste:
casovnaVrsta <- ts(rev(ceneZlata[,4]))
graf <- plot(casovnaVrsta, xlab = "Time", ylab = "EUR", main = "Zlato")
graf <- points(casovnaVrsta,pch=20)

# 2. Naloga:
# (a) Funkcija, ki časovni vrsti priredi zglajene vrednosti:

G <- function(vrsta, k){
  T <- length(vrsta)
  zglajena <- c()
  for (t in (k+1):T){
    zglajena[t] <- mean(vrsta[(t-k):(t-1)])
  }
  return(zglajena)
}


# (b) 
# Zglajena casovna vrsta z drsečim povprečjem reda 7:
k <- 7
zglajena7 <- G(casovnaVrsta, k)

# Zglajeni časovni vrsti doda napovedi:

napovedi <- function(vrsta, zglajena, k){
  T <- length(vrsta)
  for (i in 1:10){
    dodaj <- casovnaVrsta[T-k+1:T]
    napovedi <- mean(dodaj[1:k])
    zglajena[T+i] <- napovedi
  }
  return(zglajena)
}

zglajena7 <- napovedi(casovnaVrsta, zglajena7, 7)

# (c) Graf časovne vrste:

graf7 <- plot(casovnaVrsta, xlab = "Time", xlim=c(0,260), ylab = "EUR", main = "Drseče povprečje")
graf7 <- points(casovnaVrsta, pch=20)
lines(ts(zglajena7), col = "Red")

# (d) Srednja kvadratična napaka:
MSE7 <- 1 / (T - k) * sum((casovnaVrsta[(k+1):T] - zglajena7[(k+1):T])^2)

# (e):
# Red glajenja 14:
k <- 14
zglajena14 <- G(casovnaVrsta, k)

# Dodane napovedi za prihodnost:
zglajena14 <- napovedi(casovnaVrsta, zglajena14, 14)

#Srednja kvadratična napaka:
MSE14 <- 1 / (T - k) * sum((casovnaVrsta[(k+1):T] - zglajena14[(k+1):T])^2)

# Red glajenja 30:
k <- 30
zglajena30 <- G(casovnaVrsta, k)

# Dodane napovedi za prihodnost:
zglajena30 <- napovedi(casovnaVrsta, zglajena30, 30)

#Srednja kvadratična napaka:
MSE30 <- 1 / (T - k) * sum((casovnaVrsta[(k+1):T] - zglajena30[(k+1):T])^2)

# Grafi za drseče povprečje redov 7, 14 in 30:


par(mfrow=c(2,2))

graf7 <- plot(casovnaVrsta, xlab = "Time", xlim=c(0,260), ylab = "EUR", main = "Drseče povprečje reda 7")
graf7 <- points(casovnaVrsta, pch=20)
lines(ts(zglajena7), col = "Red")


graf14 <- plot(casovnaVrsta, xlab = "Time", xlim=c(0,260), ylab = "EUR", main = "Drseče povprečje reda 14")
graf14 <- points(casovnaVrsta, pch=20)
lines(ts(zglajena14), col = "Red")


graf30 <- plot(casovnaVrsta, xlab = "Time", xlim=c(0,260), ylab = "EUR", main = "Drseče povprečje reda 30")
graf30 <- points(casovnaVrsta, pch=20)
lines(ts(zglajena30), col = "Red")



# 3. Naloga: 

# a)Funkcija za enostavno eksponentno glajenje:
EG <- function(vrsta, alpha){
  T <- length(vrsta)
  zglajena <- c()
  zglajena[1] <- vrsta[1]
  for (t in 2:T){
    zglajena[t] <- alpha * vrsta[t] + (1 - alpha) * zglajena[t-1]
  }
  return(zglajena)
}

# (b) 
alpha <- 0.3
eksponentnoGlajenje <- EG(casovnaVrsta, alpha)

# Dodajanje napovedi:
for (i in 1:10){
  napoved <- alpha * casovnaVrsta[T] + (1 - alpha) * eksponentnoGlajenje[T-1]
  eksponentnoGlajenje[T+i] <- napoved
}

# Graf:
par(mfrow=c(1,1))
grafEks <- plot(casovnaVrsta, xlab = "Time", ylab = "EUR", main = "Eksponentno glajenje")
grafEks <- points(casovnaVrsta,pch=20)
lines(ts(eksponentnoGlajenje), col = "orange")

# (c) Funkcija za izračun kvadratične napake eksponentno glajene:
MSEeksponentne <- function(alpha){
  eksGlajenje <- EG(casovnaVrsta, alpha)
  MSE <- 1 / (T - 1) * sum((casovnaVrsta[2:T]- eksGlajenje[2:T])^2)
  return(MSE)
}

# Vrednost α pri kateri zglajena časovna vrsta najmanj odstopa od 
# opazovanih vrednosti, glede na kvadratično napako:
najmanseOdstopanje <- optimize(MSEeksponentne, lower=0, upper=1, maximum = FALSE)
alphaOptimalna <- najmanseOdstopanje$minimum
print(alphaOptimalna)

# (d)
glajenjeOptAlpha <- EG(casovnaVrsta, alphaOptimalna)

# Dodajanje napovedi:
for (i in 1:10){
  napoved <- alphaOptimalna * casovnaVrsta[T] + (1 - alphaOptimalna) * glajenjeOptAlpha[T-1]
  glajenjeOptAlpha[T+i] <- napoved
}

# Graf:
par(mfrow=c(1,1))
grafOptAlpha <- plot(casovnaVrsta, xlab = "Time", ylab = "EUR", main = "Eksponentno glajenje, minimalen MSE")
grafOptAlpha <- points(casovnaVrsta,pch=20)
lines(ts(glajenjeOptAlpha), col = "orange")


