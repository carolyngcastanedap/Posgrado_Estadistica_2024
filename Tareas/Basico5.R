#Importar datos
library(repmis)
url <- "https://www.dropbox.com/s/nxoijhgmutuho0s/datos_control_Rascon.csv?dl=1"
edad<-read.csv(url,header = T)
View(edad)

#Summary
summary(edad)

#Grafica 1. Independiente -2. dependiente
#Dependiente: Edad
plot(edad$DAP, edad$EDAD, pch= 19, col= "darkgreen",
     xlab = "Diametro (cm)",
     ylab = "Edad",
     ylim = c(20,140),
     xlim = c(10,50))
plot(edad$EDAD, edad$DA, pch= 19)

#correlación
#alfa=0.05
#Ho= sI HAY CORRELACIÓN
#H1= No hay correlación

cor.test(edad$DAP, edad$EDAD)

#Correlación 0.79
#Ver P para saber si es significativo, si es menor 0.001


#Regresión lineal

edad.lm <- lm(edad$EDAD ~ edad$DAP)
edad.lm

#alfa, intercepto -8.4 beta, 2.4 aumenta


#Para obtener la significancia aplico summary
summary(edad.lm)

#Residuales:diferencia entre el valor observado y el valor predicho
#coeficiente: El intercepto no es significativo, pero el beta si, valor significatos de alfa y beta, si tiene * para que sea significativo
#R-squared:Predice el valor de la variable dependiente, en este caso la edad
#Ocupa tener mas datos para que alfa sea significativo


#Linea de tendencia central
#Puede ser por que no se considera las otras variables
#Tiene que haber los mismos datos por arriba como por abajo

plot(edad$DAP, edad$EDAD, pch= 19, col= "darkgreen",
     xlab = "Diametro (cm)",
     ylab = "Edad",
     ylim = c(20,140),
     xlim = c(10,50))

abline(edad.lm)

text(20, 120, "y = -8.4 * 2.4 (x)")

#Ajustar tabla
edad.lm$coefficients
edad.lm$residuals
edad$res <- edad.lm$residuals
edad$edprim <- edad.lm$fitted.values
edad$com.res <- edad$EDAD - edad$edprim


#suma de residuales
sum(edad$res)


#Varianza 58 df- "gl"
sum(edad$res^2)
sum(edad$res^2)/58


#Estimar la edad (prima) para los valores de DAP: 15, 30, 45, 47
valores <- c(15,30,45,47)
prima <- -8.4 + 2.4*(valores)
prima

--------------------------------------------------------------------
#CASO GEYSER
#Importar datos
#Abrir archivo en excel
library(readr)
erupciones <- read_csv("erupciones.csv")
View(erupciones)

#correlación
#alfa=0.05
#Ho= sI HAY CORRELACIÓN
#H1= No hay correlación

cor.test(erupciones$waiting, erupciones$eruptions)

#Si hay correlación de 0.9 y es significativa


#Regresión lineal
erupciones.lm <- lm(erupciones$eruptions ~ erupciones$waiting)
erupciones.lm
#y = -1.87 * 0.75 (x)"


#Para obtener la significancia de alfa y beta aplico summary
summary(erupciones.lm)
#alfa y beta son sicnificativos


#Residuales:diferencia entre el valor observado y el valor predicho
#coeficiente: El intercepto no es significativo, pero el beta si, valor significatos de alfa y beta, si tiene * para que sea significativo
#R-squared:Predice el valor de la variable dependiente, en este caso la edad


#Grafico
#Linea de tendencia central, tiene que haber los mismos datos por arriba como por abajo

plot(erupciones$waiting,  erupciones$eruptions, pch= 19, col= "darkgreen",
     xlab = "Tiempo de espera",
     ylab = "Erupciones",
     ylim = c(0.5,6),
     xlim = c(40,105))

abline(erupciones.lm)

 text(50,5, "y = -1.87 * 0.75 (x)")


#Ajustar tabla

erupciones$res <- erupciones.lm$residuals
erupciones$edprim <- erupciones.lm$fitted.values
erupciones$com.res <- erupciones$eruptions - erupciones$edprim

#suma de residuales
sum(erupciones$res)


#Varianza 5270 df- "gl"
sum(erupciones$res^2)
sum(erupciones$res^2)/270


#Estimar la edad (prima) para los valores de DAP: 15, 30, 45, 47
valores <- c(15,30,45,47)
prima <- -1.87 + 0.75*(valores)
prima

