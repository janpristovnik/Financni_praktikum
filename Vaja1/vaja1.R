library(dplyr)
library(readr)
library(reshape2)


#uvozimo tabele
prva_tabela <- read.csv("Vaja1/Podatki/hist_EURIBOR_2011.csv") %>% select(X,X03.01.2011, X01.02.2011, X01.03.2011, X01.04.2011, X01.04.2011, X02.05.2011, X01.06.2011, X01.07.2011, X01.08.2011, X01.09.2011, X03.10.2011, X01.11.2011, X01.12.2011)
                                                        
druga_tabela <- read.csv("Vaja1/Podatki/hist_EURIBOR_2012.csv") %>% select(X,X02.01.2012, X01.02.2012, X01.03.2012, X02.04.2012, X02.05.2012, X01.06.2012, X02.07.2012, X01.08.2012, X03.09.2012, X01.10.2012, X01.11.2012, X03.12.2012)

tretja_tabela <- read.csv("Vaja1/Podatki/hist_EURIBOR_2013.csv") %>% select(X, X02.01.2013, X01.02.2013, X01.03.2013, X02.04.2013, X02.05.2013, X03.06.2013, X01.07.2013, X01.08.2013, X02.09.2013, X01.10.2013, X01.11.2013, X02.12.2013)

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

tabela_skupna <- rbind(prva_tabela_urejeno, druga_tabela_urejeno, tretja_tabela_urejeno)
tabela_skupna <- tabela_skupna[,c(1,2,4,5,6,9,12,15)]

casovna_vrsta1 <- ts(tabela_skupna[,6], start=c(2011,1), frequency=12)
casovna_vrsta2 <- ts(tabela_skupna[,7], start=c(2011,1), frequency=12)

vektor_imen <- c("6-mesečna obrestna mera", "9-mesečna obrestna mera")

ts.plot(casovna_vrsta1, casovna_vrsta2,main = "6-mesečna in 9-mesečna obrestna mera", xlab = "Leto", ylab = "Obrestna mera", col = c("blue","red"), lwd = 3) 
#legend('topright', vektor_imen, lty = 1, col = c("blue", "red"), lwd = 3)

tabela_urejena <- data.frame(tabela_skupna)
tabela_za_drugi_graf <- tabela_urejena[c("X01.04.2011", "X01.06.2012", "X02.05.2013"),]
