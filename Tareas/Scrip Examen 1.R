#Scrip Examen 1



# 1. Abrir un documento web
# 2. Abrir documentos de un lugar seguro
# 3. Abrir archivo en excel
# 4. Ayuda de una funcion
# 5. Ver datos
# 6. Sacar media 
# 7. Varianza
# 8. Sacar desviación estandar
# 9. Restricciones
# 10. Estadistica descriptiva (Tapply, Summary)
# 11. Determinar observaciones menores o iguales a 16.9 cm de Diamtero
# 12. Llamar a la variable como factor
# 13. Hacer un Boxplot
# 14. Realizar un histograma
# 15. Establecer hipotesis
# 16. Prueba de normalidad Shapiro
# 17. Prueba de homogenidad de varianzas
# 18. Transformar datos
# 19. Realizar analisis de variansas B4
# 20. Pruba de t independientes
# 21. Prueba t dependientes
# 22. Prueba de Tukey
# 23. Realizar plot
# 24. Correlación


# 1. Abrir un documento web
prof_url <- "http://www.profepa.gob.mx/innovaportal/file/7635/1/accionesInspeccionfoanp.csv"
profepa <- read.csv(prof_url, encoding = "latin1")
View(profepa)

#2. Abrir documentos de un lugar seguro

library(repmis)
conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")
View(conjunto)


# 3. Abrir archivo en excel, seleccionar archivo en files
prod <- read.csv("produccion.CSV", header = T)
View(prod)

# 4. Ayuda de una funcion
?tapply

# 5.Ver datos
summary(profepa)


# 6. Sacar media
mean(profepa$Inspección)

# 7. Varianza, cuanto varian los datos alrrededor de la media
var(conjunto$Altura)

# 8. Sacar desviación estandar, calcula cuanto se alejan los datos de la media
sd(conjunto$Altura)


# 9.Restricciones
ins <- subset(profepa, profepa$Inspección >= mean(profepa$Inspección))
View(ins)

bajo <- subset(profepa, profepa$Inspección <= mean(profepa$Inspección))
View(bajo)

cero <- subset(profepa, profepa$Inspección == 0)
View(cero)

todos <- subset(profepa, profepa$Inspección != mean(profepa$Inspección))
View(todos)

spFH <- subset(conjunto, conjunto$Especie != "C")
View(spFH)

# 10. Estadistica descriptiva (Tapply, Summary)
# La funcion tapply sirve para ver en este caso el promedio de una variable en especifico
# La funcion tapply sirve tambien para ver el número de factores

tapply(spFH$Diametro, spFH$Especie,mean)

tapply(madera$Peso_g,madera$Especie, mean)
tapply(madera$Peso_g,madera$Especie, var)

#Summary te muestra los datos generales
summary(madera)


# 11. Determinar observaciones menores o iguales a 16.9 cm de Diamtero

ObsDi <- subset(conjunto, conjunto$Diametro <= 16.9)
View(ObsDi)
# .......31 observaciones

# 12.Llamar a la variable como factor

conjunto$Especie <- as.factor(conjunto$Especie)
conjunto$Clase <- as.factor(conjunto$Clase)


# 13. Hacer un Boxplot, se puede hacer de una o varias variables

boxplot(spFH$Diametro ~ spFH$Especie,
        xlab = "Especies",
        ylab = "Diametro",
        col= "green")


# 14. Realizar un histograma

hist(conjunto$Altura, xlab = "altura", ylab = "frecuencia", main = "Histograma de Altura",
     xlim = c(8,24), ylim = c(0, 14), col = "darkseagreen1")

hist(conjunto$Diametro, xlab = "diametro", ylab = "frecuencia", main = "Histograma de Diametro",
     , col = "cadetblue1")

# 15. Establecer hipotesis
#Ho:Posee diferencias del peso del la madera entre Barreta y Gavia
#H1:No posee diferencias del peso del la madera entre Barreta y Gavia


# 16. Prueba de normalidad Shapiro, se acepta por que el valor de P es mayor 0.05 

shapiro.test(spFH$Diametro)

# 17. Prueba de homogenidad de varianzas, se acepta por que el valor de P es mayor 0.05

bartlett.test(spFH$Diametro, spFH$Especie)


# 18. Transformar datos, sirve para corregir las varianzas
madera$Peso_t <- log10(madera$Peso_g + 0.5)
madera$Peso_t <- log(madera$Peso_g + 0.5)
sitios$dapt <- log(sitios$DAP+1)
sitios$dapt <- sqrt(sitios$DAP)


# 19. Realizar analisis de variansas B4
sit.aov <- aov(sitios$dapt ~ sitios$Paraje)
summary(sit.aov)

#PRUEBA DE T


# 19. Pruba de t independientes, antes de hacer una prueba de T se tiene que hacer una prueba de normalidad y homogenidad de las varianzas

#DATOS QUE ARROJA:
# (Pruba de t) df son grados de libertad n-2, sirve para corroborar
# (Pruba de t) p se acepta la hipotesis nula, de qie los valores son semejantes
# (Prueba de t) de una muestra: mu- media teorica
# Se acepta la hipotesis nula por que p value es mayor 0.05

t.test(conjunto$Diametro, mu = 22)


# 20. prueba t dependientes
# La diferencia no es significativa
t.test(prod$Kgsem ~ prod$Tiempo, paired = T)


# 21. Prueba de T, con corrector de varianzas

t.test(madera$Peso_g ~ madera$Especie, var.equal = F)

# 22. Prueba de Tukey, se puede analizar con el valor de p si es mayor a 0.05, o si el lwr y el upr no hay diferencias y viceversa 
TukeyHSD(sit.aov)
plot(TukeyHSD(sit.aov))

# 23. Realizar plot
#Dependiente: Edad
plot(edad$DAP, edad$EDAD, pch= 19, col= "darkgreen",
     xlab = "Diametro (cm)",
     ylab = "Edad",
     ylim = c(20,140),
     xlim = c(10,50))
plot(edad$EDAD, edad$DA, pch= 19)


# 24. Correlación
#alfa=0.05
#Ho= sI HAY CORRELACIÓN
#H1= No hay correlación
#Correlación 0.79
#Ver P para saber si es significativo, si es menor 0.001
cor.test(edad$DAP, edad$EDAD)


