library(dplyr)
library(readr)
library(reshape2)
library(ggplot2)
#library(plyr)

#uvozimo tabele
prva_tabela <- read.csv("Vaja1/Podatki/hist_EURIBOR_2011.csv") %>% select(X,X03.01.2011, X01.02.2011, X01.03.2011, X01.04.2011, X01.04.2011, X02.05.2011, X01.06.2011, X01.07.2011, X01.08.2011, X01.09.2011, X03.10.2011, X01.11.2011, X01.12.2011)
                                                        
druga_tabela <- read.csv("Vaja1/Podatki/hist_EURIBOR_2012.csv") %>% select(X,X02.01.2012, X01.02.2012, X01.03.2012, X02.04.2012, X02.05.2012, X01.06.2012, X02.07.2012, X01.08.2012, X03.09.2012, X01.10.2012, X01.11.2012, X03.12.2012)

tretja_tabela <- read.csv("Vaja1/Podatki/hist_EURIBOR_2013.csv") %>% select(X, X02.01.2013, X01.02.2013, X01.03.2013, X02.04.2013, X02.05.2013, X03.06.2013, X01.07.2013, X01.08.2013, X02.09.2013, X01.10.2013, X01.11.2013, X02.12.2013)

#urejanje podatkov
prva_tabela_t <- t(prva_tabela)
druga_tabela_t <- t(druga_tabela)
tretja_tabela_t <- t(tretja_tabela)

imena1 <- as.character( prva_tabela_t[1,])
imena2 <- as.character( druga_tabela_t[1,])
imena3 <- as.character( tretja_tabela_t[1,])

prva_tabela_urejeno <- prva_tabela_t[-1,]
druga_tabela_urejeno <- druga_tabela_t[-1,]
tretja_tabela_urejeno <- tretja_tabela_t[-1,]

colnames(prva_tabela_urejeno) <-imena1 
colnames(druga_tabela_urejeno) <- imena2
colnames(tretja_tabela_urejeno) <- imena3

#zdruzevanje tabel in ločevanje potrebnih podatkov
tabela_skupna <- rbind(prva_tabela_urejeno, druga_tabela_urejeno, tretja_tabela_urejeno)
tabela_skupna2 <- tabela_skupna
tabela_skupna <- tabela_skupna[,c(1,2,4,5,6,9,12,15)]
tabela3 <- tabela_skupna[,c(6,7)]
tabela3[,1] <- as.numeric(as.character(tabela3[,1]))
tabela3[,2] <- as.numeric(as.character(tabela3[,2]))
#kreiranje časovne vrste za prvi graf
casovna_vrsta1 <- ts(tabela_skupna[,6], start=c(2011,1), frequency=12)
casovna_vrsta2 <- ts(tabela_skupna[,7], start=c(2011,1), frequency=12)

vektor_imen <- c("6-mesečna obrestna mera", "9-mesečna obrestna mera")

#prvi graf
ts.plot(casovna_vrsta1, casovna_vrsta2,main = "6-mesečna in 9-mesečna obrestna mera", xlab = "Leto", ylab = "Obrestna mera", col = c("blue","red"), lwd = 3) 
#legend('topright', vektor_imen, lty = 1, col = c("blue", "red"), lwd = 3)

#izbral sem si datume "01.04.2011", "01.06.2012", "02.05.2013""
#urejanje podatkov za drugi graf
tabela_urejena <- data.frame(tabela_skupna2)
tabela_za_drugi_graf <- tabela_urejena[c("X01.04.2011", "X01.06.2012", "X02.05.2013"),]

casovni_vektor = c(0.25,0.5,0.75,1,2,3,4,5,6,7,8,9,10,11,12)

tabela_za_drugi_graf <- t(tabela_za_drugi_graf)
tabela_za_drugi_graf <- data.frame(tabela_za_drugi_graf)
tabela_za_drugi_graf <- cbind(tabela_za_drugi_graf,casovni_vektor)

tabela_za_drugi_graf[,1] <- as.numeric(as.character(tabela_za_drugi_graf[,1]))
tabela_za_drugi_graf[,2] <- as.numeric(as.character(tabela_za_drugi_graf[,2]))
tabela_za_drugi_graf[,3] <- as.numeric(as.character(tabela_za_drugi_graf[,3]))
                                       
#drugi graf
drugi_graf <- plot( y = tabela_za_drugi_graf[,c(1)], x=casovni_vektor, ylim=c(min(0),max(5.5)),xlab="Dospetje [mesec]", ylab="%", col="red", main="Casovna struktura Euribor")
lines(tabela_za_drugi_graf[,c(2)], x=casovni_vektor,col="blue", type="o",pch = 16, text(10,4,"1.6.2012", col="blue"))
lines(tabela_za_drugi_graf[,c(1)], x=casovni_vektor,col="red", type="o",pch = 16, text(10,5,"1.4.2011", col="red"))
lines(tabela_za_drugi_graf[,c(3)], x=casovni_vektor,col="green", type="o",pch = 16, text(10,3,"2.5.2013", col="green"))

#racunanje L(0,T,U)
#tabela_skupna[,6] <- as.numeric(as.character(tabela_skupna[,6]))
#tabela_skupna[,7] <- as.numeric(as.character(tabela_skupna[,7]))
#v vektorju <- vetkor3 so shranjeni
Vektor <- c(0)
tabela3 <- cbind(tabela3,Vektor)

i=1
while (i<37)
  {
  tabela3[i,3] <-(1/3)*((1+9*(as.numeric(as.character(tabela3[i,2]))))/(1 + 6*(as.numeric(as.character(tabela3[i,1]))))-1)
  i = i+1
}
Euribor3m <- tabela_skupna[,5]
tabela3 <- cbind(tabela3,Euribor3m)
tabela_primerjava <- tabela3[,c(0,4,3)]
Napovedana <- c(c(NA,NA,NA,NA,NA,NA),tabela_primerjava[c(1:30),2])
tabela_primerjava[,2] <- Napovedana

#naredimo tabelo za zadnje grafe

tabela_primerjava_graf <- tabela_primerjava[c(7:36),]
tabela_primerjava_graf <- as.data.frame(tabela_primerjava_graf)
tabela_primerjava_graf[,1] <- as.numeric(as.character(tabela_primerjava_graf[,1]))
tabela_primerjava_graf[,2] <- as.numeric(as.character(tabela_primerjava_graf[,2]))

l2011 <- tabela_primerjava_graf[c(1:6),]
l2012 <- tabela_primerjava_graf[c(7:18),]
l2013 <- tabela_primerjava_graf[c(19:30),]
graf3 <- plot(x = l2011[,2], y = l2011[,1],xlab= "Napovedana", ylab = "Opazovana",  main="6m Euribor 2011-2013")
points(x=l2011[,2], y = l2011[,1], type = "p", col="red",pch = 16)
points(x=l2012[,2], y = l2012[,1], type = "p", col="blue",pch = 16)
points(x=l2013[,2], y = l2013[,1], type = "p", col="green",pch = 16)