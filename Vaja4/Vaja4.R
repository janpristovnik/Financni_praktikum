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
#a.primer
G <- function(vrsta, k){
  dolzina <- length(vrsta)  #pomagalo nam bo pri indeksiranju
  glajene_vrednosti <- c()
  for (i in 1:(dolzina-k)){
    glajene_vrednosti[i] <- sum(vrsta[i:(k+i-1)])/k
  }
  zacetno_leto <- ceiling(2012 + k/12) #ker je naš prvi podatek decembra 2012
  zacetni_mesec <- (k/12 - floor(k/12)) * 12
  zglajena_vrsta <- ts(glajene_vrednosti, start = c(zacetno_leto, zacetni_mesec), frequency = 12) #naredimo še časovno vrsto
  return(zglajena_vrsta)
}
#b.primer 

zglajena_vrsta_7 <- G(casovna_vrsta, 7)

#napoved 

dolzina <- length(casovna_vrsta)
napoved7 <- sum(casovna_vrsta[(dolzina-7+1):dolzina])/7 

#c.primer na graf dodamo še zglajeno vrsto 

graf2 <- ts.plot(casovna_vrsta, zglajena_vrsta_7, xlab = "Leto", ylab = "Vrednost zlata v evrih", main = "Drseče povprečje reda 7", col = c("gold","goldenrod3") , lwd = 3)

legend('bottomright', c('Časovna vrsta', 'Zglajena vrsta'),col = c("gold","goldenrod3"), lwd = 2:2 )

#d.primer izračun srednje kvadratične napake 

MSE <- function (vrsta,zglajena_vrsta,k) {
  T <-length(vrsta)
  delna_vsota <- 0
  for (i in (k+1) : T) {
    delna_vsota <- delna_vsota +  (vrsta[i] - zglajena_vrsta[i-k])^2
 }
 napaka <- (1/(T-k))*delna_vsota
  return( napaka)
}
Napaka7 <- MSE(casovna_vrsta, zglajena_vrsta_7, 7)

#e.primer 



#red glajenja 14
zglajena_vrsta_14 <- G(casovna_vrsta, 14)
napoved14 <- sum(casovna_vrsta[(dolzina-14+1):dolzina])/14
Napaka14 <- MSE(casovna_vrsta, zglajena_vrsta_14, 14)



#red glajenja 30 
zglajena_vrsta_30 <- G(casovna_vrsta, 30)
napoved30 <- sum(casovna_vrsta[(dolzina-30+1):dolzina])/30
Napaka30 <- MSE(casovna_vrsta, zglajena_vrsta_30, 30)

#graf 
graf2 <- ts.plot(casovna_vrsta, zglajena_vrsta_7, xlab = "Leto", ylab = "Vrednost zlata v evrih", main = "Drseče povprečje reda 7", col = c("gold","goldenrod3") , lwd = 3)

legend('bottomright', c('Časovna vrsta', 'Zglajena vrsta'),col = c("gold","goldenrod3"), lwd = 2:2 )

graf3 <- ts.plot(casovna_vrsta, zglajena_vrsta_14, xlab = "Leto", ylab = "Vrednost zlata v evrih", main = "Drseče povprečje reda 14", col = c("gold","goldenrod3") , lwd = 3)

legend('bottomright', c('Časovna vrsta', 'Zglajena vrsta'),col = c("gold","goldenrod3"), lwd = 2:2 )

graf4 <- ts.plot(casovna_vrsta, zglajena_vrsta_30, xlab = "Leto", ylab = "Vrednost zlata v evrih", main = "Drseče povprečje reda 30", col = c("gold","goldenrod3") , lwd = 3)

legend('bottomright', c('Časovna vrsta', 'Zglajena vrsta'),col = c("gold","goldenrod3"), lwd = 2:2 )


#3.naloga EKSPONENTNO GLAJENJE 

#a.primer 

EG <- function(vrsta, alpha) { 
  glajena_vrsta <- c(vrsta[1])
  dolzina = length(vrsta)
  for (i in  2:dolzina ){
    glajena_vrsta[i]<- alpha*vrsta[i] + (1 - alpha)*glajena_vrsta[i-1]
  }
  zglajena_vrsta_3 <- ts(glajena_vrsta, start = c(2013,1), frequency = 12)
  return (zglajena_vrsta_3)
}

#b.primer

#izberemo alpha = 1/4

zglajena_alpha_cetrtina <- EG(casovna_vrsta, 0.25)
napoved_cetrtina <- zglajena_alpha_cetrtina[60]

graf_6 <- ts.plot(casovna_vrsta,zglajena_alpha_cetrtina, xlab = "Leto", ylab = "Vrednost zlata v evrih", main = "Eksponentno glajenje", col =c("gold2", "black")  , lwd = 3)
legend('bottomright', c('Časovna vrsta', 'Zglajena vrsta'),col = c("gold2","black"), lwd = 2:2 )


#c.primer pomagamo si z ukazom optimize
MSE_e <- function(vrsta, alpha){
  dolzina <- length(vrsta)
  napaka <- 0
  glajena <- EG(vrsta, alpha)
  for (i in 1:(dolzina-1)){
    napaka <- napaka + (vrsta[i+1] - glajena[i+1])^2
  }
  return(napaka/(dolzina-1))
}

optimalni_alpha <- optimize(MSE_e, c(0,1), vrsta = casovna_vrsta)

#d.primer 
zglajena_alpha_optimalna <- EG(casovna_vrsta, optimalni_alpha$minimum)

graf_7 <- ts.plot(casovna_vrsta,zglajena_alpha_optimalna, xlab = "Leto", ylab = "Vrednost zlata v evrih", main = "Eksponentno glajenje, optimalni alpha", col =c("gold2", "black")  , lwd = 3)
legend('bottomright', c('Časovna vrsta', 'Zglajena vrsta'),col = c("gold2","black"), lwd = 2:2 )



