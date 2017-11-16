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
vektor_x <- seq(0,9,0.5)
graf <- plot(stepfun(vektor_x, diffinv(diskretna_y)))
curve(pweibull(x,shape,scale),add=TRUE, from = 0,to = 9, col = "red", lwd = 3)

#2.c S Panjerjevim algoritmom bom izračunal porazdelitveno funkcijo kumulativne škode S
porazdelitvena <- aggregateDist(method = "recursive",
                                model.freq = "poisson",
                                model.sev = diskretna_y,
                                x.scale = h,
                                lambda =15,
                                tol = 0.01,
                                convolve = 0
                                )

plot(porazdelitvena)

#2.d izračunaj upanje in disperzijo komulativne škode



Upanje_S_diskretno <- (vektor_x %*% diskretna_y) *15
