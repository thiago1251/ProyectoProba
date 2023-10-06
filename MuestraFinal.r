# Importacion de la base de datos
database= read.csv("Base_Datos_Proyecto_Proba_!000_Empresas_Mas_Grandres_Del_Pais.csv", sep=",", header = TRUE )
desviacion_estandar = sd(database$GANANCIA..PERDIDA..2018, na.rm = TRUE)

#MUESTREO ESTRATIFICADO
library(dplyr)
vc = qnorm(0.05/2, lower.tail = F)
n=((vc*desviacion_estandar)/(30000000))^2 #muestra

tabla = table(database$MACROSECTOR)
tabla

#Arreglo 
arrange(database,database$MACROSECTOR)

#CREAR ESTRATOS
Estrato1 = filter(database,database$MACROSECTOR == "AGROPECUARIO")
Estrato2 = filter(database,database$MACROSECTOR == "CONSTRUCCIÓN")
Estrato3 = filter(database,database$MACROSECTOR == "MINERO-HIDROCARBUROS")
Estrato4 = filter(database,database$MACROSECTOR == "COMERCIO")
Estrato5 = filter(database,database$MACROSECTOR == "MANUFACTURA")
Estrato6 = filter(database,database$MACROSECTOR == "SERVICIOS")

#Tamaño de cada estrato

N1 = 24
N2 = 66
N3 = 54
N4 = 303
N5 = 328
N6 = 225

#FRACCION DE MUESTREO N = 1000
N = 1000
n = 100
P = n/N

# No. de elementos por estrato 
n1 = floor(N1*P)
n2 = floor(N2*P)
n3 = floor(N3*P)
n4 = floor(N4*P)
n5 = floor(N5*P)
n6 = floor(N6*P)

n1+n2+n3+n4+n5+n6

set.seed(123)

parte1 = sample(1:N1,n1,replace=F)
Muestra1=Estrato1[parte1,]

parte2 = sample(1:N2,n2,replace=F)
Muestra2=Estrato2[parte2,]

parte3 = sample(1:N3,n3,replace=F)
Muestra3=Estrato3[parte3,]

parte4 = sample(1:N4,n4,replace=F)
Muestra4=Estrato4[parte4,]

parte5 = sample(1:N5,35,replace=F)
Muestra5=Estrato5[parte5,]

parte6 = sample(1:N6,n6,replace=F)
Muestra6=Estrato6[parte6,]



MFinal = rbind(Muestra1,Muestra2,Muestra3,Muestra4,Muestra5,Muestra6)
