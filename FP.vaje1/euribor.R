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

graf1 <- ts.plot(sesti, deveti, ylim=c(0,6), gpars=list(xlab= "Čas", ylab="%"),
                 col=c("red","blue"),main="Euribor obrestne mere")
graf1 <- c(text(2010.5,5.5,"Čas zapadlosti:"),text(2010.5,5,"6m",col = "red"),text(2010.5,4.5,"9m",col = "blue"))

# 2. Naloga:
izbrani <- t(tabela[c(9,14, 31),c(4:15)])
#View(izbrani)

datum1 <- ts(izbrani[,1], start = c(0,1), frequency = 2)
datum2 <- ts(izbrani[,2], start = c(0,1), frequency = 2)
datum3 <- ts(izbrani[,3], start = c(0,1), frequency = 2)
graf2 <- ts.plot(datum1, datum2, datum3, ylim=c(0,6),col=c("blue","orange","red"),
                 gpars=list(xlab= "Dospetje [mesec]", ylab="%"),main="Časovna struktura Euribor")
graf2 <- c(points(datum1, col="blue",pch=18),points(datum2,col="orange",pch=18),points(datum3,col="red",pch=18))
graf2 <- c(text(5,5,"1.9.2008",col = "blue"),text(5,2,"2.2.2009",col = "orange"),text(5,0.9,"1.7.2010",col = "red"))

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
#View(napoved)

# (c)
xos <- c(as.vector(napoved[(7:36),4]),c(4.7, 4.7, 4.7))
yos <- c(as.vector(napoved[(7:36),3]), c(1, 1.5, 2))
regresija <- lm(as.vector(napoved[(7:36),3]) ~ as.vector(napoved[(7:36),4]))
barve <- c(rep("red",6), rep("blue",12), rep("orange", 12), c("red","blue","orange"))
graf3 <- plot(xos, yos, xlim= c(0,6),ylim= c(0,6), col=barve, xlab = "Napovedana obrestna mera",
              ylab = "Opazovana obrestna mera", main = "3m Euribor 2008-2010",pch= 19, 
              abline(regresija,col="darkgreen"))
graf3 <- abline(0,1, col="grey")
graf3 <- c(text(5.2,1,"2008"),text(5.2,1.5,"2009"),text(5.2,2,"2010"))

# (d)

# Leto 2008:
xos8 <- as.vector(napoved[(7:12),4])
yos8 <- as.vector(napoved[(7:12),3])
regresija8 <- lm(yos8 ~ xos8)

graf4 <- plot(xos8, yos8, xlim= c(0,6),ylim= c(0,6), col="red", xlab = "Napovedana obrestna mera", 
              ylab="Opazovana obrestna mera", main= "3m Euribor 2008", abline(regresija8, col="red"),pch= 19)
graf4 <- abline(0,1, col="grey")

# Leto 2009:
xos9 <- as.vector(napoved[(13:24),4])
yos9 <- as.vector(napoved[(13:24),3])
regresija9 <- lm(yos9 ~ xos9)

graf5 <- plot(xos9, yos9, xlim= c(0,6),ylim= c(0,6), col="blue", xlab = "Napovedana obrestna mera", 
              ylab="Opazovana obrestna mera", main= "3m Euribor 2009", abline(regresija9, col="blue"),pch= 19)
graf5 <- abline(0,1, col="grey")

# Leto 2010:
xos10 <- as.vector(napoved[(25:36),4])
yos10 <- as.vector(napoved[(25:36),3])
regresija10 <- lm(yos10 ~ xos10)

graf6 <- plot(xos10, yos10, xlim= c(0,2),ylim= c(0,2), col="orange", xlab = "Napovedana obrestna mera", 
              ylab="Opazovana obrestna mera", main= "3m Euribor 2010", abline(regresija10, col="orange"),pch= 19)
graf6 <- abline(0,1, col="grey")