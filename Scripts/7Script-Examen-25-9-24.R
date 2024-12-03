#cAROLYN CASTAÑEDA

#16

set.seed(42)
n <- 30
altura <- rnorm(n, mean = 170, sd= 10)
altura
peso <- 0.5 * altura + rnorm(n, mean = 0, sd= 5)
peso


shapiro.test(peso)
bartlett.test(peso, altura)

t.test(altura, peso, paired = T)


# 17
cor.test(altura, peso)
IN <- lm(altura ~ peso)
IN
summary(IN)

#19-20

set.seed(25)
n <- 40
diamar <- rnorm(n, mean = 20, sd= 5) #edad de los arboles
diamar
alturar <- 1.5 * diamar + rnorm(n, mean = 0, sd= 3)# altura
alturar
# Crear data fame
datos <- data.frame(diamar,alturar)

View(datos)

LM <-lm(datos)
LM
#   ALFA    BETA  
#y = 2.12 * 0.6 (x)


