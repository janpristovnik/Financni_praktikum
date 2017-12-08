library(combinat)
library(Rlab)

#naloga 1

#1.a
S_0 <- 50
u <- 1.05
d <- 0.95
U <- 5
R <- 0.03
T <- 3

S0 <-c(50.00, 50.00, 50.00, 50.00, 50.00)
S1 <-c(52.50, 52.50, 47.50, 47.50, 52.50)
S2 <-c(48.88, 55.12, 49.88, 45.12, 49.88)
S3 <-c(52.37, 57.88, 47.38, 47.38, 52.37)
S4 <-c(49.75, 60.78, 45.01, 49.75, 54.99)
S5 <-c(52.24, 63.81, 42.76, 52.24, 57.74)
Izplacilo_X <-c(0, 0, 0, 0, 0)
Izplacilo_Y <-c(0, 0, 0, 0, 0)




tabela1 <- data.frame(S0, S1, S2, S3, S4, S5,  Izplacilo_X, Izplacilo_Y)

for (i in 1:5){
  l = max( max(tabela1[i,(T+1):(U+1)])  -   max(tabela1[i, 1:T]), 0) 
  tabela1$Izplacilo_X[i] <- l
}

for (i in 1:5){
  k = max( min(tabela1[i,(T+1):(U+1)])  -   min(tabela1[i, 1:T]), 0) 
  tabela1$Izplacilo_Y[i] <- k
}

#1.b 

izplacilo <- function(vrsta, T, type = c("call", "put") ) {
  if (type == "call") {
  return( max( max(vrsta[(T+1):length(vrsta)])  -   max(vrsta[1:T]), 0))}
  else {
  return( max( min(vrsta[(T+1):length(vrsta)])  -   min(vrsta[1:T]), 0) )}
}

#2.naloga

binomski <- function(S0,u,d,U,R,T,type){
  q = (1+R-d)/(u-d)
  razpleti <- hcube(rep(2,U), translation = -1)
  razpleti_1 <- d**razpleti * u**(1-razpleti)
  S0_vektor <- rep(S_0, 2**U)
  razpleti_2 <- cbind(S0_vektor, razpleti_1)
  apply()
}
  


  


