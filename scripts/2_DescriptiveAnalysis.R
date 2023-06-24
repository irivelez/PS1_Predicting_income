#########################################################################################
##############          Taller 1 - Big data and Machine Learning           ##############
#########################################################################################

############################    Integrantes del grupo       #############################

# Irina Andrea Vélez López – Código:  
# Miguel Angel Victoria Simbaqueva – Código:  
# Daniel Casas Bautista – Código: 202120803
# Lucia Fillippo – Código: 202213187

#######################    Adecuación del espacio de trabajo      ########################

install.packages("pacman") #Instalar librería si no contamos con esta 
library(pacman) #Llamar librería
p_load("tidyverse","stargazer", "rvest")
rm(list = ls()) #Limpia las variables que existan al momento de correr el código

#######################    Scraping de las bases de datos a usar    ######################

#NOTA: La matriz queda guardada y la podemos llamar, para no tener que correr ese código demorado
DatosGEIH<-readRDS("Datos_GEIH.Rds") #Para cargar la base

#Importamos cada base de datos y la volvemos data.frame para poder convertirlos en una  matriz
Base1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")%>%
  html_table()
Base1 <- data.frame(Base1)
Base2 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html")%>% 
  html_table()
Base2 <- data.frame(Base2)
Base3 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html")%>% 
  html_table()
Base3 <- data.frame(Base3)
Base4 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html")%>% 
  html_table()
Base4 <- data.frame(Base4)
Base5 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html")%>% 
  html_table()
Base5 <- data.frame(Base5)
Base6 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html")%>% 
  html_table()
Base6 <- data.frame(Base6)
Base7 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html")%>% 
  html_table()
Base7 <- data.frame(Base7)
Base8 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html")%>% 
  html_table()
Base8 <- data.frame(Base8)
Base9 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html")%>% 
  html_table()
Base9 <- data.frame(Base9)
Base10 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html")%>% 
  html_table()
Base10<- data.frame(Base10)
#Al observar que cada una de las bases de datos si pudo ser importada, se procede a unir cada base de datos
#Con la fusión de todas las bases de datos, tendremos oficialmente los Datos completos de la GEIH de 2018
DatosGEIH<- rbind(Base1, Base2, Base3, Base4, Base5, Base6, Base7, Base8, Base9, Base10)

#Con este código guardamos y cargamos la base de datos para no tener que hacer siempre el Scraping

#DatosGEIH<-readRDS("Datos_GEIH.Rds") #Para cargar la base, en caso que no se tenga tiempo para cargar desde la página web-Solo se debe poner la base de datos en la ubicación de la sesión y carga esta tabla
#saveRDS(DatosGEIH, file = "Datos_GEIH.rds") #Crea el archivo RDS en el directorio de trabajo, en caso de necesitarse


#######################    Ordenar la base de datos      ########################

# Aquí incluimos solo a las personas con edad mayor o igual a 18 años
DatosGEIH_18<-DatosGEIH[DatosGEIH$age>=18,]

# Aquí hacemos la clasificación de variables
exp <- floor(c(DatosGEIH_18$p6426/12)) #Ponemos anual la variable de experiencia
view(exp)   #Vemos que hay un montón de observaciones "NA" que nos piden quitar
educ <- DatosGEIH_18$p6210 #Se asigna la variable educación
View(educ)
DGEIH<-subset(DatosGEIH_18, select = c( "ingtot", "pet", "mes", "age", "sex","ocu", "oficio") ) #Hacemos un subset con las variables a usar
DGEIH<-cbind(DGEIH, exp, educ) #Incluimos las variables calculadas que utilizaremos en el modelo
View(DGEIH) # Aquí aún no hemos quitado las observaciones "NA" 
DGEIH<- DGEIH[DGEIH$ingtot>0,]  #Este ingreso no tiene "NA" pero sí tiene ceros, aquí los limpiamos
summary(DGEIH) #Visualización general de la base de datos... peeero, no está horario el salario
DGEIH<- DGEIH[(DGEIH$ingtot)/744,]  #Este ingreso no tiene "NA" pero sí tiene ceros, aquí los limpiamos
length(DGEIH$ingtot)
# Aquí la idea es quitar las observaciones "NA"
cantidad_na <- sapply(DGEIH, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(DGEIH)
porcentaje_na <-porcentaje_na*100
porcentaje_na #Visualizamos el porcentaje de los datos que tienen NA
DGEIH$oficio[is.na(DGEIH$oficio)] = 100 #Se imputa la nueva categoría 1oo a Oficio

DGEIH <- DGEIH %>% #Se vuelven categóricas las variables que así lo sean en la BD
  mutate_at(.vars = c(
    "pet","sex", "ocu", "educ", "oficio"),
    .funs = factor)

DGEIH[is.na(DGEIH)] = 0 #Se asigna 0 a las NA de la variable "exp"
#DGEIH %>% subset(ingtot <= 2*iqr | is.na(ingtot)==T)
summary(DGEIH) #Se verifica que no existan NA
View(DGEIH) #Se verifica que no existan NAs

#######################    Análisis descriptivo      ########################

# Descripción general de la base
View(DGEIH)
nrow(DGEIH) #Número de filas
ncol(DGEIH) #Número de columnas
dim(DGEIH)  #Número de filas y columnas
head(DGEIH) #Esto muestra los primeros valores de la base... no sirve mucho pero me pareció cool
tail(DGEIH) #Esto muestra los últimos  valores de la base... no sirve mucho pero me pareció cool

# Descripción de la variable de edad
Edad<- DGEIH$age
class(Edad)
plot(hist(Edad))    #Este diagrama de barras podemos incluirlo en el doc
mean(Edad)          #Edad media
min(Edad)           #Edad mínima (recordemos que no incluimos a gente menor a 18 años)
max(Edad)           #Edad máxima... 106 años, caramba... para ponerlo en el doc
mean(Edad)
modeEdad <- function(Edad){
  return(as.numeric(names(which.max(table(Edad)))))
}
modeEdad(Edad)      #La moda de la edad

# Descripción de la Población en Edad de Trabajar (PET)
PET <- DGEIH$pet
View(DGEIH$pet)
class(PET)
levels(PET)
summary(PET)        #Todas las observaciones son 1 (esta es una dummy que dice que están en edad de trabajar, por eso es siempre 1)

# Descripción de la variable de educación
plot(hist(educ))
class(educ)
mean(educ)
modeEduc <- function(educ){
  return(as.numeric(names(which.max(table(educ)))))
}
modeEduc(educ)

# Descripción de la variable de ocupación
ocu<- DGEIH$ocu
class(ocu)
levels(ocu)         
summary(ocu)      
table(ocu)        #A diferencia de PET que es siempre 1, aquí si vemos que algunos son 0
pie(table(ocu))   #Aquí podemos poner un pie que se ve genial

# Descripción de la variable de género
sex<- DGEIH$sex
class(sex)
levels(sex)
summary(sex)
table(sex)
barplot(table(sex))
pie(table(sex))   #Podemos mirar si diagrama de barras o un pie (voto por el pie)

# Descripción de la variable de experiencia
expp<- DGEIH$exp
View(expp)
class(expp)
plot(hist(expp))
mean(expp)
min(expp)
max(expp)
modeExp <- function(expp){
  return(as.numeric(names(which.max(table(expp)))))
}
modeExp(expp)

# Descripción de la variable de oficio
library(modeest)
Oficio_<- DGEIH$oficio
class(Oficio_)
levels(Oficio_)
summary(Oficio_)
table(Oficio_)
barplot(table((Oficio_)))
mlv(Oficio_, method = "mfv")

