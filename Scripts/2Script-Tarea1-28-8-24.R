# Carolyn Gineth Castañeda Poveda
# Asignación 2: Uso de restricciones y estadísticas descriptivas

# 1. Abrir archivo en excel
# 2. Importar la base de datos a excel a R en un objeto llamada conjunto

conjunto <- read.csv("Tarea1.CSV", header = T)
View(conjunto)

# 3. Aplicar la función subset para la variable Altura de acuerdo a las siguintes indicaciones

#     -Incluir los datos iguales o menores a la media (objeto en R se llame: H.media)

H.media <- subset(conjunto, conjunto$Altura <= mean(conjunto$Altura))
mean(conjunto$Altura)
#   13.94256
View(H.media)

#     -Incluir los datos menores a 16.5 m (objeto en R se llame: H.16)

H.16 <- subset(conjunto, conjunto$Altura < 16.5)
View(H.16)


# 4. Aplicar la función subset para la variable Vecinos

#     - Incluir los árboles que tengan un número de vecinos iguales o menores a 3 (Objeto en R:vecinos3)

Vecinos3 <- subset(conjunto, conjunto$Vecinos <= 3)
View(Vecinos3)

#     - Incluir los árboles que tengan un número de vecinos mayores a 4 (Objeto en R: Vecinos4)

Vecinos4 <- subset(conjunto, conjunto$Vecinos > 4)
View(Vecinos4)


# 5.Aplicar la función subset para la variable Diametro

#     - Incluir los diámetros menores a la media (objeto en R: DBH-media)

mean(conjunto$Diametro)
DBHmedia <- subset(conjunto, conjunto$Diametro < mean(conjunto$Diametro))
View(DBHmedia)

# Incluir los diámetros mayores a 16 (Objeto en R DBH-16)

DBH16 <- subset(conjunto, conjunto$Diametro > 16)
View(DBH16)

# 6.Aplicar la función subset para la variable Especie

#    -Incluir la especie Cedro Rojo

EspmC <- subset(conjunto, conjunto$Especie == "C")
View(EspmC)

#    -Incluir la especie Tsuga heterófila y Douglasia verde

EspmTD <- subset(conjunto, conjunto$Especie != "C")
View(EspmTD)

# 7.Determinar cuantas observaciones son menores o iguales a 16.9 cm de Diamtero

ObsDi <- subset(conjunto, conjunto$Diametro <= 16.9)
View(ObsDi)
# 31 observaciones

# 8. Determinar cuantoas observacions son mayores a 18.5 metros de Altura

ObsAl <- subset(conjunto, conjunto$Altura > 18.5)
View(ObsAl)
# 2 observaciones


# Visualización de datos

# 9.Con la función hist generar los histogramas para los objetos creados en el apartado anterior

#     Altura
hist(conjunto$Altura, xlab = "altura", ylab = "frecuencia", main = "Histograma de Altura",
     xlim = c(8,24), ylim = c(0, 14), col = "darkseagreen1")
#     H.media
hist(H.media$Altura, xlab = "altura", ylab = "frecuencia", main = "Histograma de Altura - H.media",
     xlim = c(8,14), ylim = c(0, 8), col = "darkseagreen3")
#     H.16
hist(H.16$Altura, xlab = "altura", ylab = "frecuencia", main = "Histograma de Altura - H.16",
     xlim = c(8,18), ylim = c(0, 13), col = "darkseagreen4")
#     vecinos
hist(conjunto$Vecinos, xlab = "vecinos", ylab = "frecuencia", main = "Histograma de Vecinos",
     xlim = c(0,6), ylim = c(0, 15), col = "bisque")
#     vecinos3
hist(Vecinos3$Vecinos, xlab = "vecinos", ylab = "frecuencia", main = "Histograma de Vecinos3",
     col = "bisque2")
#     vecinos4
hist(Vecinos4$Vecinos, xlab = "vecinos", ylab = "frecuencia", main = "Histograma de Vecinos4",
     , col = "bisque4")
#     Diametro
hist(conjunto$Diametro, xlab = "diametro", ylab = "frecuencia", main = "Histograma de Diametro",
    , col = "cadetblue1")
#     DBH-media
hist(DBHmedia$Diametro, xlab = "diametro", ylab = "frecuencia", main = "Histograma de Diametro - DBHmedia",
     , col = "cadetblue3")
#     DBH-16
hist(DBH16$Diametro, xlab = "diametro", ylab = "frecuencia", main = "Histograma de Diametro - DBH16",
     , col = "cadetblue4")

# Estadísticas básicas

# 10. Determinar la media (mean) de los objetos (variable y respectivos subsets), así como su desviación estándar (sd).

#     Altura
mean(conjunto$Altura)
sd(conjunto$Altura)

#     H.media 
mean(H.media$Altura)
sd(H.media$Altura)

#     H.16
mean(H.16$Altura)
sd(H.16$Altura)

#     Vecinos 
mean(conjunto$Vecinos)
sd(conjunto$Vecinos)

#    Vecinos-3
mean(Vecinos3$Vecinos)
sd(Vecinos3$Vecinos)

#    Vecinos-4
mean(Vecinos4$Vecinos)
sd(Vecinos4$Vecinos)

#    Diametro
mean(conjunto$Diametro)f
sd(conjunto$Diametro)

#   DBH-media
mean(DBHmedia$Diametro)
sd(DBHmedia$Diametro)

#   DBH-16
mean(DBH16$Diametro)
sd(DBH16$Diametro)
