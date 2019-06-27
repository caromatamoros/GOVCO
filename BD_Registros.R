# Para resetear el estado 

rm(list=ls())
gc()


#Para cargar los paquetes - No todos se usan pero es útil tenerlos en la construcción
library(data.table)
library(ipapi)
library(xml2)
library(XML)
library(rvest)
library(stringr)
library(httr)
library(dplyr)
library(maps)
library(mapproj)
library(rversions)
library(writexl)
library(ggplot2)

#Para fijar el directorio de trabajo - Establece la carpeta donde se descargaría o cargaría la informaciÃ³n

#setwd("C:\\Users\\cmatamoros\\OneDrive - Ministerio de Tecnologías de la Información y las Comunicaciones\\Gov Co\\Datos Integración\\2019-06-25")

#Para descargar el archivo de datos abiertos como viene

excel <- read.csv("https://www.datos.gov.co/api/views/x33q-mn74/rows.csv?accessType=DOWNLOAD")
excel <- excel[!excel$nombre_entidad == "-",]
# en caso exista una observación vacía
excel <- excel[1:nrow(excel)-1,]


#Para usar un archivo descargado con anterioridad
datos_abiertos <- read.csv(file="20190619 1425 Plan_de_Integraci_n.csv", header=TRUE, sep=",")
datos_abiertos <- datos_abiertos[complete.cases(datos_abiertos[ , 2]),]
#Define la matriz de datos abiertos como la descargada directamente

datos_abiertos <- excel
#View(datos_abiertos)

#?gsub

#Crea la matriz de data no raw sobre la que se limplia la informaciÃ³n
colnames(datos_abiertos)
datos2 <- data.frame(matrix("", ncol = ncol(datos_abiertos), nrow = nrow(datos_abiertos)))
colnames(datos2) <- colnames(datos_abiertos)
test <- data.frame()
#rm(n)
i <- double()


#ciclo para limpiar los errores derivados de caracteres especiales

for (i in 1:ncol(datos2)) {
  
  test <- gsub("Ã¡","á", 
               gsub("Ã©","é",  
                    gsub("Ã-","í",       
                         gsub("Ã³","ó",
                              gsub("Ãº","ú",     
                                   gsub("Ã‘","Ñ",    
                                        gsub("â€“","–",    
                                             gsub("Ã±","ñ",datos_abiertos[,i])))))))) 
    
  datos2[,i]<- test

}


#Columna que incluye los criterios de evaluación de si la información de un trámite está completa

#Un trámite está completo si:
# -	Si el tipo de acción es acondicionar adicional a las demás variables requiere definir tipo de integración.
# -	Si el tipo de acción es eliminar no necesita fecha
#Adicional a los casos especiales en general requiere de acción, fecha, responsable de trámite y correo del responsable del trámite.

Estado_Tramite <- data.frame(matrix("",nrow = nrow(datos_abiertos),ncol = 1))

i <- double()

for (i in 1:nrow(Estado_Tramite)) {

  Estado_Tramite[i] <- 
    ifelse(datos2$nombre_tramite_servicio[i]=="","no hay trámite",
       ifelse(datos2$acciones[i]=="","no hay acción",       
               ifelse(datos2$acciones[i]=="Acondicionar",
                          ifelse(datos2$integracion[i]=="","no hay tipo de integración",
                                 ifelse(datos2$fecha[i]=="None","no hay fecha",
                                        ifelse(datos2$responsable_tramite[i]=="","no tiene responsable",
                                               ifelse(datos2$correo_tramite[i]=="","no tiene correo responsable","completo")))),
                    ifelse(datos2$acciones[i]=="Eliminar",
                           ifelse(datos2$responsable_tramite[i]=="","no tiene responsable",
                                  ifelse(datos2$correo_tramite[i]=="","no tiene correo responsable","completo")),
                           ifelse(datos2$fecha[11]=="None","no hay fecha",
                                  ifelse(datos2$responsable_tramite[i]=="","no tiene responsable",
                                         ifelse(datos2$correo_tramite[i]=="","no tiene correo responsable","completo")))))))
                             
} 

Estado_Tramite <- as.data.frame(t(Estado_Tramite),stringsAsFactors = FALSE)       
Estado_Tramite <- Estado_Tramite$V1

#Aquí se vincula la columna a la base de datos tratada
datos2 <- cbind(datos2,Estado_Tramite)


#Columna que incluye los criterios de evaluación de si la información de un dominio está completa

Estado_Dominio <- data.frame(matrix("",nrow = nrow(datos_abiertos),ncol = 1))

i <- double()

for (i in 1:nrow(Estado_Dominio)) {
  
  Estado_Dominio[i] <- 
    ifelse(datos2$nombre_sitio_web[i]=="","no hay dominio",
           ifelse(datos2$integracion[i]=="","no hay tipo de integración",       
                  ifelse(datos2$integracion[i]=="Eliminar",
                         ifelse(datos2$fecha[i]=="None","no hay fecha","completo"),
                  ifelse(datos2$integracion[i]=="Permanece","completo",
                         ifelse(datos2$tipo_sitio_web[i]=="","no hay tipo de sitio web",
                                ifelse(datos2$fecha[i]=="None","no hay fecha","completo"))))))
                                      
} 

Estado_Dominio <- as.data.frame(t(Estado_Dominio),stringsAsFactors = FALSE)       
Estado_Dominio <- Estado_Dominio$V1

#Aquí se vincula la columna a la base de datos tratada
datos2 <- cbind(datos2,Estado_Dominio)

#Columna agregada de criterios de completo
# Esta evalua que si es un medio, esta completo o si las variables de estado dominio o estado de trámites están completas, está completo

Estado_Rev <- data.frame(matrix("",nrow = nrow(datos_abiertos),ncol = 1))

i <- double()

for (i in 1:nrow(Estado_Rev)) {
  
  Estado_Rev[i] <- 
    ifelse(datos2$Estado_Tramite[i]=="completo"|datos2$Estado_Dominio[i]=="completo","completo",
           ifelse(datos2$tipo[i]=="otros medios","completo","incompleto"))
} 

Estado_Rev <- as.data.frame(t(Estado_Rev),stringsAsFactors = FALSE)       
Estado_Rev <- Estado_Rev$V1

#Aquí se vincula la columna a la base de datos tratada
datos2 <- cbind(datos2,Estado_Rev)


#getwd()
NombreArchivo <- paste(Sys.Date(),"Consolidado Datos Integración.xlsx",sep=" ") 
write_xlsx(datos2,NombreArchivo)




