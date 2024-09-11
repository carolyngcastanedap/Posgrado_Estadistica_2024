#Importar datos
url <- "https://raw.githubusercontent.com/mgtagle/Met_Est_2024/main/Datos_Madera_MET.csv"
madera<-read.csv(url,header = T)
View(madera)

#1. Establecer hipotesis 
#Ho:Posee diferencias del peso del la madera entre Barreta y Gavia
#H1:No posee diferencias del peso del la madera entre Barreta y Gavia

#2.Grafico de variables y niveres de factor
hist(madera$Peso_g ~ madera$Repeticion)
boxplot(madera$Peso_g ~madera$Especie,
        xlab = "factor",
        ylab = "peso",
        col= "green")

#3.Estadistica descriptiva
tapply(madera$Peso_g,madera$Especie, mean)
tapply(madera$Peso_g,madera$Especie, var)
summary(madera)

#4. Cuantos factores de nivel exsisten?

#Son 2 factores (Especies: Barreta  y Gavia )

# La varianza de Gavia es 7 veces mayor

#5.Prueba de normalidad Shapiro, se acepta por que el valor de P es mayor 0.05 , es normal

shapiro.test(madera$Peso_g)

#6.Prueba de homogenidad de varianzas, se rechaza por que el valor de de P es menor 0.05, las varianzas son diferentes

bartlett.test(madera$Peso_g ~ madera$Especie)

#7. Transformar datos
madera$Peso_t <- log10(madera$Peso_g + 0.5)
madera$Peso_t <- log(madera$Peso_g + 0.5)

tapply(madera$Peso_t,madera$Especie, var)

boxplot(madera$Peso_t ~madera$Especie,
        xlab = "factor",
        ylab = "peso",
        col= "green")

#8.Aplicar prueba de T, con corrector de varianzas

t.test(madera$Peso_g ~ madera$Especie, var.equal = F)

#9.Aceptar o negar hipotesis

#Se rechaza hipotesis nula


#---------------------------------------------------------------------------
#Abrir archivo en excel
sitios <- read.csv("Rascon.CSV", header = T)
View(sitios)

#Estadistica descriptiva
tapply(sitios$DAP, sitios$Paraje, mean)
tapply(sitios$DAP, sitios$Paraje, var)

#Boxplot de la edad pr sitio
boxplot(sitios$DAP ~ sitios$Paraje,
        xlab = "Sitio",
        ylab = "Edad",
        col= "green")

tapply(sitios$EDAD, sitios$Paraje, mean)
tapply(sitios$EDAD, sitios$Paraje, var)

#Prueba de normalidad Shapiro, se rechaza por que el valor de P es menor 0.05 , no es normal

shapiro.test(sitios$DAP)

#Prueba de homogenidad de varianzas, se aceta por que el valor de de P es mayor 0.05, las varianzas son semejantes

bartlett.test(madera$Peso_g ~ madera$Especie)

#Transformar datos

sitios$dapt <- log(sitios$DAP+1)
sitios$dapt <- sqrt(sitios$DAP)

shapiro.test(sitios$dapt)

#Grafico

boxplot(sitios$dapt ~ sitios$Paraje,
        xlab = "Sitio",
        ylab = "Edad",
        col= "green")

#Sacar media
mean(sitios$dapt^2)

#Realizar analisis de variansas
sit.aov <- aov(sitios$dapt ~ sitios$Paraje)
summary(sit.aov)

#Prueba de tuke, se pued eanalizar con el valor de p si es mayor a 0.05, o si criza lwr y el upr no hay diferencias y viceversa, trinidad es el que tiene los mejores diametros es el mayor
TukeyHSD(sit.aov)
plot(TukeyHSD(sit.aov))
