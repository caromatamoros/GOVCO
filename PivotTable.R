# Para resetear el estado 

rm(list=ls())
gc()

#Para fijar el directorio de trabajo
#setwd("C:\\Users\\cmatamoros\\OneDrive - Ministerio de Tecnologías de la Información y las Comunicaciones\\Gov Co\\Datos Integración\\2019-06-25")

#Corre la lectura de datos abiertos

#source("Pruebas.r")

library(shiny)
library(htmlwidgets)
library(pivottabler)
library(basictabler)
library(readxl)

#source("UI-TablaDinamica.r")

td <- PivotTable$new()
td$addData(datos2)
td$addColumnDataGroups("tipo")
td$addColumnDataGroups("Estado_Rev")
td$addRowDataGroups("nombre_entidad")
td$defineCalculation(calculationName="Total Acciones", summariseExpression="n()")
#td$evaluatePivot()
td$renderPivot()
tabla <- td$asDataFrame()


#proceso para limpiar los errores derivados de caracteres especiales

names(tabla)
test <- data.frame()

test <- gsub("Ã¡","á", 
             gsub("Ã©","é",  
                  gsub("Ã-","í",       
                       gsub("Ã³","ó",
                            gsub("Ãº","ú",     
                                 gsub("Ã‘","Ñ",    
                                      gsub("â€“","–",    
                                           gsub("Ã±","ñ",names(tabla))))))))) 
  
names(tabla)<- test

test <- data.frame()

test <- gsub("Ã¡","á", 
             gsub("Ã©","é",  
                  gsub("Ã-","í",       
                       gsub("Ã³","ó",
                            gsub("Ãº","ú",     
                                 gsub("Ã‘","Ñ",    
                                      gsub("â€“","–",    
                                           gsub("Ã±","ñ",rownames(tabla))))))))) 

rownames(tabla)<- test

#Reemplazar N/A por 0
tabla[is.na(tabla)] <- 0

#Incluir variable de Porcentaje de Avance

Porcentaje_Avance <- data.frame(matrix("",nrow = nrow(tabla),ncol = 1))
names(Porcentaje_Avance)<- "Porcentaje_Avance"

i <- double()

for (i in 1:ncol(tabla)) {
  tabla[,i] <- as.integer(tabla[,i])
}
i <- double()
for (i in 1:nrow(tabla)) {
    Porcentaje_Avance[i] <- (tabla[i,1]+tabla[i,6])/(tabla[i,3]+tabla[i,8])
} 
Porcentaje_Avance<- as.data.frame(t(Porcentaje_Avance),stringsAsFactors = FALSE)       
Porcentaje_Avance <-Porcentaje_Avance$V1

tabla <- cbind(tabla,Porcentaje_Avance)


Base_datos <- read_excel("20190626 - Base de Datos SoporteCCC.xlsx")


#Instrucciones para complementar la base de datos conforme a los resultados obtenidos

#1) Quienes no han usado la herramienta

UsoHerramienta <- vector(length = nrow(Base_datos))

i <- double()
for (i in 1:nrow(Base_datos)) {
  UsoHerramienta[i] <- is.element(Base_datos$Nombre[i], rownames(tabla))
} 

Base_datos <- cbind(Base_datos,UsoHerramienta)

# 2) Porcentaje de avance

Base_datos[,"Porcentaje de Avance"] <- 0
i <- double()
for (i in 1:nrow(Base_datos)){
  if(is.element(Base_datos$Nombre[i], rownames(tabla))){
    #temp<- Base_datos$Nombre[1]
    #temp2 <- tabla[temp,10] 
    Base_datos$`Porcentaje de Avance`[i] <- tabla[Base_datos$Nombre[i],10] 
  }
}

# 3) Finalizaron el ejercicio

Base_datos[,"Finalizaron"] <- FALSE
i <- double()
for (i in 1:nrow(Base_datos)){
  if(Base_datos$`Porcentaje de Avance`[i]==1){
    Base_datos$`Finalizaron`[i] <- TRUE 
  } 
}

# 4) Que han diligenciado y tienen un porcentaje entre 0 y 50%

Base_datos[,"Entre 0 y 50%"] <- FALSE
i <- double()
for (i in 1:nrow(Base_datos)){
  if(Base_datos$`Porcentaje de Avance`[i]<=0.5){
    Base_datos$`Entre 0 y 50%`[i] <- TRUE
  }
  if (Base_datos$UsoHerramienta[i] == FALSE){
    Base_datos$`Entre 0 y 50%`[i] <- FALSE
  }  
}

# 5) Que no han finalizado y tienen un porcentaje superior al 50%

Base_datos[,"Entre +50% y <100%"] <- FALSE
i <- double()
for (i in 1:nrow(Base_datos)){
  if(Base_datos$`Porcentaje de Avance`[i]>0.5&Base_datos$`Porcentaje de Avance`[i]<1){
    Base_datos$`Entre +50% y <100%`[i] <- TRUE
  }  
}

NohanIngresado <- Base_datos[Base_datos$UsoHerramienta == FALSE, ]$Nombre

Finalizaron <- Base_datos[Base_datos$Finalizaron == TRUE, ]$Nombre

Entre0y50 <- Base_datos[Base_datos$`Entre 0 y 50%` == TRUE, ]$Nombre

Entre50y100 <- Base_datos[Base_datos$`Entre +50% y <100%`== TRUE, ]$Nombre

length(NohanIngresado)+length(Finalizaron)+length(Entre0y50)+length(Entre50y100)

tabla <- cbind(rownames(tabla),tabla)
colnames(tabla)[1] <- "Entidades"

ArchivoTablaDinamica <- paste(Sys.Date(),"tabla_dinamica.xlsx",sep=" ") 
write_xlsx(tabla,ArchivoTablaDinamica)


ResumenEntidades <- paste(Sys.Date(),"Resumen_Entidades.xlsx",sep=" ") 
write_xlsx(Base_datos,ResumenEntidades)
