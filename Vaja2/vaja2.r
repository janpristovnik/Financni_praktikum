library(actuar)
#Uvozimo podatke in narišemo histogram 1.a
vzorec <- scan("Vaja2/Podatki/vzorec1.txt")



#1.b izberem Weibulovo porazdelitev, saj pri parametrih lambda =1, k=1.5
#njena gosota najbolj spominja na histogram, ki sem ga dobil iz vzorca

Vektor1 <- mde(vzorec, pweibull, start = list(shape = 1, scale = 1), measure = "CvM")
shape <- Vektor1$estimate[1]
scale <- Vektor1$estimate[2]

#1.c

histogram <- hist(vzorec, probability = TRUE, xlab = "Visina odskodnine", ylab = "Frequency")
curve(dweibull(x,shape,scale),add=TRUE, from = 0,to = 9)

histogram2 <- plot(ecdf(vzorec), main = " Porazdelitvena funkcija odskodnin ", ylab = "Porazdelitvena funkcija", xlab = "Visina odskodnine")


#1.d Za porazdelitev števila odškodninskih zahtevkov bomo vzeli: pois( lambda = 15 )
lambda <- 15
Upanje_Y <- scale*gamma(1 + 1/shape)
#Upanje S
Upanje_S <- lambda*Upanje_Y
Varianca_Y <-  (scale)^2 * (gamma(1 + 2/shape) - (gamma(1+1/shape))^2)
#Varianca S
Varianca_S <- lambda*Varianca_Y + (Upanje_Y)^2 * lambda 

#2.naloga
h = 0.5
n = 19


#2.b
diskretna_y <- discretize(1 - exp(-(x/scale)^shape),from = 0, to= h*n , step = h ,method = "rounding")
diskretna_y1 <- discretize(1 - exp(-(x/scale)^shape),from = 0, to= 1000 , step = h ,method = "rounding")
vektor_x <- seq(0,9,0.5)
graf <- plot(stepfun(vektor_x, diffinv(diskretna_y)))
curve(pweibull(x,shape,scale),add=TRUE, from = 0,to = 9, col = "red", lwd = 2)

#2.c S Panjerjevim algoritmom bom izračunal porazdelitveno funkcijo kumulativne škode S
porazdelitvena <- aggregateDist(method = "recursive",
                                model.freq = "poisson",
                                model.sev = diskretna_y1,
                                x.scale = h,
                                lambda =15,
                                tol = 0.002,
                                convolve = 0
                                )

plot(porazdelitvena)

#2.d izračunaj upanje in disperzijo komulativne škode Var(s) = var(y)*E(N) + E(y)^2*var(N)



Upanje_S_diskretno <- (vektor_x %*% diskretna_y) *15
E_y <- vektor_x %*% diskretna_y
E_ykvadrat <- (vektor_x^2 %*% diskretna_y)
Var_y <- E_ykvadrat - E_y ^2 

Var_S <- Var_y * 15 + E_y ^2 * 15

#2.e

odst_995 <- VaR(porazdelitvena, 0.995)
izpad_005 <- CTE(porazdelitvena, 0.005)

#3.naloga
#3.a 

vektor_N <- rpois(10000, 15)
vektor_vmesni <- c(0)
vektor_S <- vector(mode= "numeric", length = 10000)
stevec =1
for (i in vektor_N) {
  vektor_vmesni <- rweibull(i, shape, scale)
  vektor_S[stevec] <- sum(vektor_vmesni)
  stevec = stevec +1
}

#vektor_S

#2.b

Upanje_simulacija = mean(vektor_S)
Variacija_simulacija = var(vektor_S)

#upanje podobno, varianca razlikuje kar precej -> zelo podobno varianci iz 1.naloge


