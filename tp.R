#-------------------------------------------------
# Trabajo Practico N°1
# Integrantes: Tomas Santucci y Fernando Silva
# ------------------------------------------------

# Librerias
library(knitr)


getwd()
setwd("/home/silva/Escritorio/TP-R")

# leemos el csv de recorridos
recorridos <- read.csv("./recorridos6.csv")

# leemos el csv de usuarios
usuarios <- read.csv("./usuarios6.csv")

# combinamos los csv
bicicletas <- merge(usuarios, recorridos, all = TRUE)
# damos un orden a los dias
dia <- factor(bicicletas$dia, levels = c("Lunes","Martes","Miércoles","Jueves", "Viernes", "Sábado", "Domingo"))

# ====================================================

# VARIABLE GENERO
# Utilizamos un grafico de torta para representar 
# el porcentaje de usuarios que utilizan el sistema
# segun su genero

var_genero <- function(){
  genero <- table(usuarios$genero_usuario)[2:4]
  
  label <- c("Femenino","Otro","\n\nMasculino")
  porcentajes <- round((genero/sum(genero))*100, digits = 2)
  label <- paste(label,"\n\t",porcentajes,"%")
  color <- c("#FFC09F","#A0CED9","#FFEE93")
  
  pie(
    genero,
    labels = label,
    col = color
  )
}

var_genero()

#=================================================

# VARIABLE EDAD
# Utilizamos un grafico de histograma y graficos de poligonos de frecuencia
# para representar la variacion de edad de los usuarios del sistema

edad <- usuarios$edad_usuario
breakpoints <- c(15,20,25,30,35,40,45,50,55,60,72)

titulo <- "EDAD DE LOS USUARIOS DE ECOBICI\nCABA 2020"

# Histograma

hist(edad,breaks = breakpoints, right = TRUE,
     xlim = c(10,80), ylim = c(0,0.05), xlab = "Edad", ylab = "Densidad",
     main = titulo, col = "pink")



rng_edad <- cut(edad, breakpoints, right = TRUE)
tabla_rng_edad <- table(rng_edad)
n <- sum(tabla_rng_edad)

# Poligono de frecuencias
x <- breakpoints
y <- c(0,tabla_rng_edad)/n
plot(x,y,type="l", xlim = c(15,72), ylim = c(0,0.25),
     main = titulo,
     ylab = "Frecuencia relativa", xlab = "Edad",lwd = 1.5)
for (i in seq(0,0.25,by=0.05)) abline(a=i,b=0,lty=2,lwd=0.5)


# Poligono acumulativo
y = cumsum(y)
plot(x,y,type="l", xlim = c(15,72), main = titulo,
     ylab = "Frecuencia relativa acumulada", xlab = "Edad", lwd = 1.5)
for (i in seq(0,1,by=0.2)) abline(a=i,b=0,lty=2,lwd=0.5)

# ============================================
# DIAS
# Para representar la frecuencia de uso de las bicicletas
# segun el dia de la semana utilizamos un barplot

semana <- table(dia)

barplot(semana, ylim = c(0,150), col = c("lightblue"))

#===============================================

# ESTACION DE ORIGEN
# Representamos la estacion de origen con un barplot
# y adicionalmente mostramos una tabla de las frecuencias del mismo

frecAbs <- table(bicicletas$direccion_estacion_origen)
frecAbs <- sort(frecAbs, decreasing = TRUE)
frecAbs <- frecAbs[1:15]
frecAbs <- sort(frecAbs, decreasing = FALSE)
frecRel <- frecAbs/sum(frecAbs)
frecAbsAcum <- cumsum(frecAbs)
frecRelAcum <- cumsum(frecRel)
frecRel <- round(frecRel, digits = 4)
frecRelAcum <- round(frecRelAcum, digits = 4)

tabla <- cbind(frecAbs,frecRel,frecAbsAcum,frecRelAcum)
kable(tabla, caption = "Tabla de frecuencias")

par(mar = c(4,13,4,4))
barplot(
  frecAbs,
  horiz = TRUE,
  las = 1,
  xlim = c(0,25),
  cex.names = 0.6,
  col = "pink",
)
par(mar = c(5,4,4,2) + 0.1)

#===============================================

# ESTACION DE DESTINO
# Representamos la estacion de destino con un barplot
# y adicionalmente mostramos una tabla de las frecuencias del mismo

frecAbs <- table(bicicletas$direccion_estacion_destino)
frecAbs <- sort(frecAbs, decreasing = TRUE)
frecAbs <- frecAbs[1:15]
frecAbs <- sort(frecAbs, decreasing = FALSE)
frecRel <- frecAbs/sum(frecAbs)
frecAbsAcum <- cumsum(frecAbs)
frecRelAcum <- cumsum(frecRel)
frecRel <- round(frecRel, digits = 4)
frecRelAcum <- round(frecRelAcum, digits = 4)

tabla <- cbind(frecAbs,frecRel,frecAbsAcum,frecRelAcum)
kable(tabla, caption = "Tabla de frecuencias", )

par(mar = c(4,14,4,4))
barplot(
  frecAbs,
  horiz = TRUE,
  las = 1,
  xlim = c(0,25),
  cex.names = 0.6,
  col = "pink",
)
par(mar = c(5, 4, 4, 2) + 0.1)

#==========================================

# DISTANCIA
# vamos a mostrar la frecuencia de la variable distancia
# con un boxplot

distancia <- bicicletas$distancia/1000
distancia <- distancia[distancia<20]
summary(distancia)

boxplot(distancia, horizontal = TRUE, outline = TRUE,
        xlab = "Distancia en kilometros", boxfill = "pink")

#===============================================

# DURACION
# vamos a mostrar la frecuencia de la variable 
# Duracion con un Boxplot

duracion <- bicicletas$duracion_recorrido/60
summary(duracion)
boxplot(duracion, horizontal = TRUE, outline = FALSE,
        xlab = "Duracion en minutos", boxfill = "pink")

# ========================================

# CANTIDAD DE VIAJES POR USUARIO
# Vamos a mostrar la cantidad de viajes por usuarios
# con un grafico de baras

viajes <- table(bicicletas$id_usuario)
plot(table(viajes), ylim = c(0,100), xlab = c("Viajes") ,ylab = c("Cantidad De Viajes"))

# =============================
# Analisis Bivariado
# ===============================

# Joven -> 16 a 21 
# Adulto -> 21 30
# Adulto Mayor -> 30 a 45
# Mayor Mayor -> 45 a 72

edad <- bicicletas$edad_usuario

agrupar <- function(x){
  if (x <= 21) {
    return ("Joven")
  } else if(x <= 30) {
    return ("Adulto")
  } else if(x <= 45){
    return ("Adulto Mayor")
  } else if(x <= 72){
    return("Mayor Mayor")
  }
}

edad <- sapply(edad,agrupar)
distancia <- bicicletas$distancia/1000

bivar <- data.frame(edad,distancia)

bivar$edad <- factor(bivar$edad, levels = c("Joven","Adulto","Adulto Mayor","Mayor Mayor"))

boxplot((bivar$distancia)~(bivar$edad), horizontal = TRUE, outline = FALSE,
        xlab = "Distancia en KM", boxfill = "pink")
