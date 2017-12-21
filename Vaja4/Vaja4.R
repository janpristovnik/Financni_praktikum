library(Quandl)
library(dplyr)

#1.naloga 

#a.naloga -> Uvoz podatkov 

tabela1 <- Quandl("LBMA/GOLD", collapse="monthly", start_date="2012-12-07") %>%
  subset(select = c(1,6))
tabela1 <- tabela1[-c(1),]

#b.naloga -> Nariši graf časovne vrste 

cena_zlata <- rev(tabela1[,2]) #morali smo obrniti podatke 



casovna_vrsta <- ts(cena_zlata, start = c(2012,12), frequency = 12 ) #naredimo časovno vrsto 
graf_zlata <- ts.plot(casovna_vrsta, xlab = "Leto", ylab = "Vrednost zlata v evrih", main = "Graf vrednosti zlata", col = "gold2" , lwd = 3)

#2.naloga 

G <- function(vrsta, k){
  dolzina <- length(vrsta)
  glajene_vrednosti <- c()
  for (i in 1:(dolzina-k)){
    glajene_vrednosti[i] <- sum(vrsta[i:(k+i-1)])/k
  }
    
    
    }
 
