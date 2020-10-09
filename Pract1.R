install.packages("ggplot2")
library(ggplot2)
install.packages("moments")
library(moments)
install.packages("psych")
library(psych)
install.packages("stringr")          
library("stringr")  


setwd("C:/Users/munek/Desktop/Superior/5to Sem/Estadística/practica1")
P1 = read.table("AHG_Practica1.csv", header=F, sep="," , dec=".")
P2 = read.table("alan.csv", header=F, sep="," , dec=".")
P3 = read.table("archivo para practica 1 Mitzi.csv", header=F, sep="," , dec=".")
P4 = read.table("Archivo_Practica1.csV", header=F, sep="," , dec=".")
P5 = read.table("archivoparapractica1.csv", header=F, sep="," , dec=".")
P6 = read.table("BasilioPPractica.csv", header=F, sep="," , dec=".")
P7 = read.table("Datos Práctica 1.csv", header=F, sep="," , dec=".")
P8 = read.table("Datos_1.csv", header=F, sep="," , dec=".")
P9 = read.table("Estadistica.csV", header=F, sep="," , dec=".")
P10 = read.table("fer2.csv", header=F, sep="," , dec=".")
P11 = read.table("Jerson.csv", header=F, sep="," , dec=".")
P12 = read.table("jorge.csv", header=F, sep="," , dec=".")
P13 = read.table("karina lizeth ortiz munoz - Hoja 1.csv", header=F, sep="," , dec=".")
P14 = read.table("LAHF.csV", header=F, sep="," , dec=".")
P15 = read.table("Libro3.csv", header=F, sep="," , dec=".")
P16 = read.table("Nombre.csv", header=F, sep="," , dec=".")
P17 = read.table("OCJM-2016.csv", header=F, sep="," , dec=".")
P18 = read.table("Pract.csv", header=F, sep="," , dec=".")
P19 = read.table("Practica 1 (1).csV", header=F, sep="," , dec=".")
P20 = read.table("Practica 1 (2).csv", header=F, sep="," , dec=".")
P21 = read.table("Práctica 1..csv", header=F, sep="," , dec=".")
P22 = read.table("Practica 1.csv", header=F, sep="," , dec=".")
P23 = read.table("Práctica 1.csv", header=F, sep="," , dec=".")
P24 = read.table("Practica 1__.csV", header=F, sep="," , dec=".")
P25 = read.table("Práctica.1..csv", header=F, sep="," , dec=".")
P26 = read.table("Practica.1.csv", header=F, sep="," , dec=".")
P27 = read.table("Practica_1 (1).csv", header=F, sep="," , dec=".")
P28 = read.table("Practica_1.csv", header=F, sep="," , dec=".")
P29 = read.table("Práctica_1.csV", header=F, sep="," , dec=".")
P30 = read.table("Práctica_1_.csv", header=F, sep="," , dec=".")
P31 = read.table("practica01.csv", header=F, sep="," , dec=".")
P32 = read.table("Practica1 (1).csv", header=F, sep="," , dec=".")
P33 = read.table("Practica1 (2).csv", header=F, sep="," , dec=".")
P34 = read.table("Practica1 (3).csV", header=F, sep="," , dec=".")
P35 = read.table("Practica1 (4).csv", header=F, sep="," , dec=".")
P36 = read.table("practica1 augusto.csv", header=F, sep="," , dec=".")
P37 = read.table("Practica1.csv", header=F, sep="," , dec=".")
P38 = read.table("Practica1.Juan Carlos.csv", header=F, sep="," , dec=".")
P39 = read.table("Practica1_.csV", header=F, sep="," , dec=".")
P40 = read.table("practica1C.csv", header=F, sep="," , dec=".")
P41 = read.table("practica-1502.csv", header=F, sep="," , dec=".")
P42 = read.table("sergio2.csv", header=F, sep="," , dec=".")
P43 = read.table("t2.csV", header=F, sep="," , dec=".")
P44 = read.table("ulises.csv", header=F, sep="," , dec=".")



total = rbind(P1, P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,P22,P23,P24,P25,P26,P27,
              P28,P29,P30,P31,P32,P33,P34,P35,P36,P37,P38,P39,P40,P41,P42,P43,P44)
names(total)= c("S","E","C")
total[2,2]=21
total$E <- as.numeric(as.character(total$E)) 
total[4,1]="H"

########################################################################
#EJERCICIO 1

z <- total["E"]
boxplot(z, col = c("purple"))

quantile(z, prob = c(0.25, 0.5, 0.75), na.rm = TRUE)

IQR(total$E)

a <- quantile(total$E, prob = c(0.25)) - 1.5*IQR(total$E)
a

b <- quantile(total$E, prob = c(0.75)) + 1.5*IQR(total$E)
b

######################################################################
#EJERCICIO 2

#DESVIACIÓN ESTÁNDAR
sd(total$E, na.rm = TRUE)

#COEFICIENTE DE ASIMETRÍA
skewness(z)

#COEFICIENTE DE SIMETRÍA
#skew(z)

#COEFICIENTE DE CURTOSIS
kurtosi(z)




######################################################################
#EJERCICIO 3


porcentajes <- as.numeric(round(((prop.table(table(total$S)))*100),2))
#porcentajes

etiquetas <- c("Hombre", "Mujer")
#etiquetas

etiquetas <- paste(etiquetas, porcentajes)
#etiquetas

#GRAFICA
pie(porcentajes,etiquetas)


######################################################################
#EJERCICIO 4

sum(str_detect(total$C, "hotmail", negate = FALSE))
sum(str_detect(total$C, "gmail", negate = FALSE))
sum(str_detect(total$C, "comunidad", negate = FALSE))
sum(str_detect(total$C, "outlook", negate = FALSE))

cor <- c(sum(str_detect(total$C, "hotmail", negate = FALSE)),sum(str_detect(total$C, "gmail", negate = FALSE)),
         sum(str_detect(total$C, "comunidad", negate = FALSE)), sum(str_detect(total$C, "outlook", negate = FALSE)))
#cor

correos <- c("hotmail","gmail","comunidad","outlook")

grafc = barplot(height=cor, names=correos, horiz=1, las=1, col=c('red','black','green','blue'))




######################################################################
#EJERCICIO 5

#Crear una grafica de barras vertical de los dominios por sexo.

hotm <- sum(str_detect(total$C, "hotmail", negate = FALSE) & str_detect(total$S, "M", negate = FALSE))
hoth <- sum(str_detect(total$C, "hotmail", negate = FALSE) & str_detect(total$S, "H", negate = FALSE))

gmm <- sum(str_detect(total$C, "gmail", negate = FALSE) & str_detect(total$S, "M", negate = FALSE))
gmh <- sum(str_detect(total$C, "gmail", negate = FALSE) & str_detect(total$S, "H", negate = FALSE))

com <- sum(str_detect(total$C, "comunidad", negate = FALSE) & str_detect(total$S, "M", negate = FALSE))
coh <- sum(str_detect(total$C, "comunidad", negate = FALSE) & str_detect(total$S, "H", negate = FALSE))

outm <- sum(str_detect(total$C, "outlook", negate = FALSE) & str_detect(total$S, "M", negate = FALSE))
outh <- sum(str_detect(total$C, "outlook", negate = FALSE) & str_detect(total$S, "H", negate = FALSE))

h = c(hoth,gmh,coh,outh)
#h
m = c(hotm,gmm,com,outm)
#m

tab = data.frame (rbind(m,h))
names(tab)= c("hotmail","gmail","comunidad","outlook")


juntos = rbind(h,m)
#juntos

barplot(height=juntos, names=correos, las=1, col=c('blue','purple'))
######################################################################
