# Finančni praktikum: OBRESTNW KRIVULJE, Ana Golob


# 1. Naloga:
# b) Tabele s podatki Euribor za leta 2008, 2009 in 2010:

euribor2008 <- read.table("Copyofhist_EURIBOR_2008.txt", header = TRUE, dec = '.')
tabela2008 <- euribor2008[, c(1,23,44,63,85,106,127,150,171,193,216,236)]
tabela2008 <- t(tabela2008)
#View(tabela2008)

euribor2009 <- read.table("Copyofhist_EURIBOR_2009.txt", header = TRUE, dec = '.')
tabela2009 <- euribor2009[, c(1,22,42,64,84,104,126,149,170,192,214,235)]
tabela2009 <- t(tabela2009)
#View(tabela2009)

euribor2010 <- read.table("Copyofhist_EURIBOR_2010.txt", header = TRUE, dec = '.')
tabela2010 <- euribor2010[, c(1,21,41,64,84,105,127,149,171,193,214,236)]
tabela2010 <- t(tabela2010)
#View(tabela2010)

# Združena tabela: izbrala sem 6. in 9. mesec.

tabela <- rbind(tabela2008, tabela2009, tabela2010)
#View(tabela)

mojiMeseci <- tabela[, c(9, 12)]
#View(mojiMeseci)


# c) Grafični prikaz tabele:

sesti <- ts(mojiMeseci[,1], start= c(2008,1),frequency = 12)
deveti <- ts(mojiMeseci[,2],start= c(2008,1), frequency = 12)

graf1 <- ts.plot(sesti, deveti, ylim=c(0,6), gpars=list(xlab= "Meseci", ylab="Obrestne mere"),col=c(25,30),main="Euribor obrestne mere")




# 3.Naloga: HIPOTEZA PRIČAKOVANJ TRGA

napovedi <- rep(NA,36)
for (i in 1:30){
  terminskaO = 100 * ((1 + 0.09 * mojiMeseci[i,2])/(1 + 0.06 * mojiMeseci[i,1]) - 1) / 4
  napovedi[i + 6] <- terminskaO
}
print(napovedi)


# (b) Tabela opazovane 3 mesečne obrestne mere in njene napovedi:

napoved <- cbind(mojiMeseci,tabela[,6], napovedi[1:36])
colnames(napoved) <- c("Euribor6m", "Euribor9m","Euribor3m", "napoved3m")
View(napoved)

# (c) 
xos <- as.vector(napoved[(7:36),3])
yos <- as.vector(napoved[(7:36),4])
print(yos)
plot(xos, yos, xlim= c(0,6), ylim= c(0,6), xlab = "Opazovana obrestna mera", ylab = "Napovedana obrestna mera", main = "Odvisnost napovedanih in opazovanih obrestnih mer",pch= 19 )
