#Llamar un documento web

prof_url <- "http://www.profepa.gob.mx/innovaportal/file/7635/1/accionesInspeccionfoanp.csv"
profepa <- read.csv(prof_url, encoding = "latin1")
head(profepa)
View(profepa)

#Datos
summary(profepa)

#Media
mean(profepa$Inspección)

#restricción
#subset conjuntos para filtrar

ins <- subset(profepa, profepa$Inspección >= mean(profepa$Inspección))
View(ins)

bajo <- subset(profepa, profepa$Inspección <= mean(profepa$Inspección))
View(bajo)

cero <- subset(profepa, profepa$Inspección == 0)
View(cero)

todos <- subset(profepa, profepa$Inspección != mean(profepa$Inspección))
View(todos)

#Ayuda de una fucion
?tapply

#-------------------------------------------------------------------------

# Descargar de datos seguro

library(repmis)
conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")
View(conjunto)

#Llamar a la variable como factor
conjunto$Especie <- as.factor(conjunto$Especie)
conjunto$Clase <- as.factor(conjunto$Clase)

#Ver datos
summary(conjunto)

#Restricción
spFH <- subset(conjunto, conjunto$Especie != "C")
View(spFH)

tapply(spFH$Diametro, spFH$Especie,mean)

#boxplot

boxplot(spFH$Diametro ~ spFH$Especie)

boxplot(conjunto$Diametro ~ conjunto$Especie)

#perzonalizar boxplot

boxplot(spFH$Diametro ~ spFH$Especie,
        xlab = "Especies",
        ylab = "Diametro",
        col= "green")

#Prueba de normalidad Shapiro, se acepta por que el valor de P es 0.05 

shapiro.test(spFH$Diametro)

#Prueba de homogenidad de varianzas

bartlett.test(spFH$Diametro, spFH$Especie)

#Pruba de t independientes

t.test(spFH$Diametro ~ spFH$Especie, var.equal = TRUE)

# (Pruba de t) df son grados de libertad n-2, sirve para corroborar
# (Pruba de t) p se acepta la hipotesis nula, de qie los valores son semejantes


# Prueba de t de una muestra
# mu- media teorica
# Se acepta la hipotesis alternativa por que p value es menor 0.05

t.test(conjunto$Diametro, mu = 22)

# Se acepta la hipotesis alternativa por que p value es menor 0.05

t.test(conjunto$Diametro, mu = 17)

# Se acepta la hipotesis nula por que p value es mayor 0.05

t.test(conjunto$Diametro, mu = 16.4)

#-----------------------------------------------------------------------------
#Abrir archivo en excel
prod <- read.csv("produccion.CSV", header = T)
View(prod)

#boxplot
boxplot(prod$Kgsem ~ prod$Tiempo)

#prueba t Dependientes
# La diferencia no es significativa
t.test(prod$Kgsem ~ prod$Tiempo, paired = T)

# ver media 
tapply(prod$Kgsem,prod$Tiempo, mean)
