# autorización de vinculo de cuenta y de computador
#rsconnect::setAccountInfo(name='caromatamoros', token='9B5C3B834BFA2AEF61F434DD17A66401', secret='jtZpDLap4H7ecqHCkUUqkp6nVmiy7zd+/ksPwVPz')
#setwd("C:\\Users\\cmatamoros\\OneDrive - Ministerio de Tecnologías de la Información y las Comunicaciones\\Gov Co\\Datos Integración\\2019-06-25")

#Inicializar
rm(list=ls())
gc()

library(shiny)
library(shinydashboard)
library(rsconnect)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)


#Leer los productos de Pruebas y Pivot Table 
TablaDinamica <- read_excel(paste("2019-06-27","tabla_dinamica.xlsx",sep=" "))
Entidades <- read_excel(paste("2019-06-27","Resumen_Entidades.xlsx",sep=" "))
DatosLimpios <- read_excel(paste("2019-06-27","Consolidado Datos Integración.xlsx",sep=" "))
#Sys.Date()
#Crear los Gráficos asociados al reporte de la Ministra

# 1) Pie con la cantidad de Registros incluye tanto los diligenciados como los incompletos

TotalesTipo <- 
    DatosLimpios %>% # Base general 
    group_by(tipo) %>% # Agrupada por tipo
      summarize(n()) # Agregada

colnames(TotalesTipo) <- c("Tipo","cantidad")
TotalesTipo <- cbind (TotalesTipo,TotalesTipo$cantidad/sum(TotalesTipo$cantidad))
colnames(TotalesTipo) <- c("Tipo","cantidad", "porcentaje")


pie1 <-  plot_ly(TotalesTipo, labels = ~Tipo, values = ~cantidad, type = 'pie',textposition = 'outside',textinfo = 'label+percent+value') %>%
    layout(title = 'Registros Totales',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

pie1  


# 2) Diligenciamiento agregado de Dominios y Trámites

TotalesEstado <-
DatosLimpios %>% # Base general 
  group_by(Estado_Rev,tipo) %>% # Agrupada por tipo
  summarize(n()) # Agregada

colnames(TotalesEstado) <- c("Estado","Tipo","cantidad")

TotalesEstado <-TotalesEstado[!TotalesEstado$Tipo=="otros medios",]
TotalesEstado[,"Porcentaje"] <- 0

for (i in 1:nrow(TotalesEstado)){
  if(TotalesEstado$Tipo[i]=="dominios"){
    TotalesEstado$Porcentaje[i] <-TotalesEstado$cantidad[i]/sum(TotalesEstado[TotalesEstado$Tipo=="dominios",]$cantidad) 
  }else{
    TotalesEstado$Porcentaje[i] <-TotalesEstado$cantidad[i]/sum(TotalesEstado[TotalesEstado$Tipo=="trámites",]$cantidad) 
  }
}

# b <- 
#   plot_ly(
#   y = TotalesEstado[!TotalesEstado$Estado=="completo",]$Tipo,
#   x = TotalesEstado[!TotalesEstado$Estado=="completo",]$Porcentaje,
#   name = "Incompletos",
#   type = "bar", orientation = "h",
#   marker = list(color = "rgb(255, 0, 0)",
#                 line = list(color = "rgb(20, 20, 20)",
#                             width = 2))
#   ) 
# ### Grouped Bar Chart
# b2 <- add_trace(b,
#   y = TotalesEstado[TotalesEstado$Estado=="completo",]$Tipo,
#   x = TotalesEstado[TotalesEstado$Estado=="completo",]$Porcentaje,
#   name = "Completos",
#   type = "bar", orientation = "h",
#   marker = list(color = "rgb(195, 195, 195)",
#                 line = list(color = "rgb(20, 20, 20)",
#                             width = 2)))
# ### Stacked Bar Chart 
# b3 <- layout(b2, barmode = "stack")
# b3
  
bar1 <- 
 plot_ly(TotalesEstado,
    y = TotalesEstado[!TotalesEstado$Estado=="completo",]$Tipo,
    x = TotalesEstado[!TotalesEstado$Estado=="completo",]$Porcentaje,
    name = "Incompletos",
    type = "bar", orientation = "h",
    marker = list(color = "rgb(0, 0, 400)",
    line = list(color = "rgb(20, 20, 20)",
    width = 2))) %>% 
  add_trace(
    y = TotalesEstado[TotalesEstado$Estado=="completo",]$Tipo,
    x = TotalesEstado[TotalesEstado$Estado=="completo",]$Porcentaje,
    name = "Completos",
    type = "bar", orientation = "h",
    marker = list(color = "rgb(195, 195, 195)",
    line = list(color = "rgb(20, 20, 20)",
    width = 2))) %>% 
    layout(barmode = "stack")
bar1

Sys.setenv("plotly_username"="caromatamoros")
Sys.setenv("plotly_api_key"="T0hjslho0EGN9x2Hsud0")

# httpRequestSocket.write('{"x":3,"y":1}\n')

options(browser = 'false')
chart_link <- api_create(bar1, filename="bar-stacked")
chart_link
chart_link$web_url

chart_link2 <- api_create(pie1, filename="pie general")
chart_link2
chart_link2$web_url


# 3) Pie de entidades

Entidades_sub <- cbind(Entidades[,2],Entidades[,13:17])
x <- as.data.frame((table(Entidades_sub[,2])))
x <- cbind(x,as.data.frame(table(Entidades_sub[,4]))[,2])
x <- cbind(x,as.data.frame(table(Entidades_sub[,5]))[,2])
x <- cbind(x,as.data.frame(table(Entidades_sub[,6]))[,2])
x <- as.data.frame(x)
rownames(x) <- as.character(x[,1])
x <- x[,2:5]
colnames(x) <- c("UsoHerramienta","Finalizaron","Entre0y50","Entre50y100")

x <- as.data.frame(t(x))
colnames(x) <- c("Falso","Verdadero")

for(i in 1:ncol(x)){x[,i] <- as.numeric(as.character(x[,i]))}

x["NoUso",] <- 0
x["NoUso",1] <- x["UsoHerramienta",2]
x["NoUso",2] <- x["UsoHerramienta",1]

x <- x[2:5,] 
#x <- t(x)
x <-as.data.frame(x)

pie2 <-  plot_ly(x, labels = rownames(x), values = ~Verdadero, type = 'pie',textposition = 'outside',textinfo = 'label+percent+value') %>%
  layout(title = 'Distribución Entidades',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

pie2  


chart_link3 <- api_create(pie2, filename="pie entidades")
chart_link3
chart_link3$web_url

# 4) Resultados MinTIC

MinTIC<-data.frame()

MinTIC["Dominios",] <- 0
MinTIC["Trámites",] <- 0
MinTIC["Otros Medios",] <- 0

MinTIC[,"Incompletos"] <- 0
MinTIC[,"Completos"] <- 0
MinTIC[,"Registrados"] <- 0

y <- TablaDinamica[102,]

MinTIC$Completos <- c(y$`dominios completo`,y$`trámites completo`,y$`otros medios completo`)
MinTIC$Incompletos <- c(y$`dominios incompleto`,y$`trámites incompleto`,0)
MinTIC$Registrados <- c(y$`dominios Total`, y$`trámites Total`, y$`otros medios Total`)
#Chart <- 

MinTIC 

barTIC <- 
  plot_ly(MinTIC, x = c("Dominios","Tramites","Otros Medios"), y = ~Completos  , type = 'bar', name = 'Completos') %>%
  add_trace(y = ~Incompletos, name = 'Incompletos') %>%
  layout(title = "Resultados MinTIC",yaxis = list(title = ''), barmode = 'stack')

barTIC

chart_link4 <- api_create(barTIC, filename="Resultados MinTIC")
chart_link4
chart_link4$web_url

# 5) Trasmites Desagregados por Diligenciamiento, Acciones y Tipo de Integración

BaseTramites <-
  DatosLimpios %>%  # Base general 
  filter(tipo == "trámites") %>% #filtrar a solo trámites
  group_by(Estado_Rev,acciones,integracion) %>% # Agrupada por Diligenciamiento, Acciones y Tipo de Integración
  summarize(n()) # Agregada

colnames(BaseTramites) <- c("Diligenciamiento","Acciones","Acondicionamiento","Total")

pie3 <-  
  plot_ly(BaseTramites, labels = ~Diligenciamiento, values = ~Total, type = 'pie',textposition = 'outside',textinfo = 'label+percent+value') %>%
  layout(title = 'Diligenciamiento Trámites',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
pie3

BaseTramites_C <- BaseTramites %>%  # Base general 
  filter(Diligenciamiento == "completo")

pie4 <-  
  plot_ly(BaseTramites_C, labels = ~Acciones, values = ~Total, type = 'pie',textposition = 'outside',textinfo = 'label+percent+value') %>%
  layout(title = 'Acciones Trámites',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
pie4

BaseTramites_A <- BaseTramites_C %>%  # Base general 
  filter(Acciones == "Acondicionar")

pie5 <-  
  plot_ly(BaseTramites_A, labels = ~Acondicionamiento, values = ~Total, type = 'pie',textposition = 'outside',textinfo = 'label+percent+value') %>%
  layout(title = 'Acondicionamiento Trámites',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
pie5

chart_link5 <- api_create(pie3, filename="Diligenciamiento Trámites")
chart_link5
chart_link5$web_url

chart_link6 <- api_create(pie4, filename="Acciones Trámites")
chart_link6
chart_link6$web_url

chart_link7 <- api_create(pie5, filename="Acondicionamiento Trámites")
chart_link7
chart_link7$web_url

# 6) Dominios Desagregados por Diligenciamiento y Tipo de Integración

BaseDominios <-
  DatosLimpios %>%  # Base general 
  filter(tipo == "dominios") %>% #filtrar a solo trámites
  group_by(Estado_Rev,integracion) %>% # Agrupada por Diligenciamientoy Tipo de Integración
  summarize(n()) # Agregada

colnames(BaseDominios) <- c("Diligenciamiento","Tipo de Integración","Total")

pie6 <-  
  plot_ly(BaseDominios, labels = ~Diligenciamiento, values = ~Total, type = 'pie',textposition = 'outside',textinfo = 'label+percent+value') %>%
  layout(title = 'Diligenciamiento Dominios',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
pie6

BaseDominios_C <- BaseDominios %>%  # Base general 
  filter(Diligenciamiento == "completo")

pie7 <-  
  plot_ly(BaseDominios_C, labels = BaseDominios_C$`Tipo de Integración`, values = ~Total, type = 'pie',textposition = 'outside',textinfo = 'label+percent+value') %>%
  layout(title = 'Tipo de Integración de los Dominios',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
pie7


chart_link8 <- api_create(pie6, filename="Diligenciamiento Dominios")
chart_link8
chart_link8$web_url

chart_link9 <- api_create(pie7, filename="Integración Dominios")
chart_link9
chart_link9$web_url

# 7) Otros medios desagregados por Tipo 

BaseMedios <-
  DatosLimpios %>%  # Base general 
  filter(tipo == "otros medios") %>% #filtrar a solo trámites
  group_by(tipo_medio) %>% # Agrupada por Tipo
  summarize(n()) # Agregada

colnames(BaseMedios) <- c("Tipo","Total")

pie8 <-  
  plot_ly(BaseMedios, labels = ~Tipo, values = ~Total, type = 'pie',textposition = 'outside',textinfo = 'label+percent+value') %>%
  layout(title = 'Tipos de Otros Medios',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
pie8

chart_link10 <- api_create(pie8, filename="Otros Medios")
chart_link10
chart_link10$web_url


# 8) Acciones de Dominios y Medios a realizar en el tiempo 

#Base agregada por fecha primero por tipo 

BaseFechasA <-  
  DatosLimpios %>%  # Base general 
  filter(Estado_Rev == "completo", tipo=="trámites") %>% #filtrar a solo trámites
  group_by(tipo, acciones,integracion,fecha) %>% # Agrupada por Diligenciamientoy Tipo de Integración
  summarize(n())  # Agregada
  
BaseFechasB <-
  DatosLimpios %>%  # Base general 
  filter(Estado_Rev == "completo", tipo=="dominios") %>% #filtrar a solo trámites
    group_by(tipo, acciones,integracion,fecha) %>% # Agrupada por Diligenciamientoy Tipo de Integración
  summarize(n())  # Agregada
 
# Equivalencia de nombre y variables de totales diferenciadas para el gráfico

colnames(BaseFechasA) <- c("Tipo","Acciones","Integración","Fecha","TotalA")
BaseFechasA <- subset(BaseFechasA, select = -c(Tipo))
BaseFechasA[,"TotalB"] <- 0 

colnames(BaseFechasB) <- c("Tipo","Acciones","Integración","Fecha","TotalB")
BaseFechasB <- subset(BaseFechasB, select = -c(Tipo))
BaseFechasB[,"TotalA"] <- 0 

#Base Agregada

BaseFechas <- rbind(BaseFechasA, BaseFechasB)

#Gráfico por acciones a realizar por dominio y trámite

barFechas <- 
  plot_ly(BaseFechas, x = ~Fecha, y = ~TotalB  , type = 'bar', name = 'Dominios') %>%
  add_trace(y = ~TotalA, name = 'Trámites') %>%
  layout(title = "Acciones por Fecha",yaxis = list(title = ''), barmode = 'group')
barFechas

chart_link10 <- api_create(barFechas, filename="Acciones por Fecha")
chart_link10
chart_link10$web_url

# 9) Acciones en Dominios y Trámites por fecha desagregados

#Creación Totales por categoría

BaseFechas[,"Dominios_Integrar_a_GovCo"] <- 0
BaseFechas[,"Dominios_Eliminar"] <- 0
BaseFechas[,"Dominios_Permanece"] <- 0 
BaseFechas[,"Dominios_Integrar_SitioWeb"] <- 0 
BaseFechas[,"Tramites_Ac_Publicación_Ficha"] <- 0 
BaseFechas[,"Tramites_Ac_Interfaz_Gráfica"] <- 0
BaseFechas[,"Tramites_Ac_WEB_API"] <- 0
BaseFechas[,"Tramites_Ac_Desarrollo_Integrado"] <- 0
BaseFechas[,"Tramites_Eliminar"] <- 0
BaseFechas[,"Tramites_Transformar"] <- 0

# Ciclos para otorgar el valor conforme a la categoría

i <- double()

for (i in 1:nrow(BaseFechas)){
  
  if(is.na(BaseFechas$Acciones[i])){ #Dominios
    
    if(BaseFechas$Integración[i]=="Eliminar"){ #Dominios_Eliminar
      BaseFechas$Dominios_Eliminar[i] <- BaseFechas$TotalB[i]
      
    } else if(BaseFechas$Integración[i]=="Integrar a GOV.CO") { #Dominios_Integrar a GovCo
      BaseFechas$Dominios_Integrar_a_GovCo[i] <- BaseFechas$TotalB[i]
    
    } else if(BaseFechas$Integración[i]=="Integrar al sitio oficial") { #Dominios_Integrar la sitio oficial
      BaseFechas$Dominios_Integrar_a_GovCo[i] <- BaseFechas$TotalB[i]
    
    } else { #Dominios_Permanece
      BaseFechas$Dominios_Permanece[i] <- BaseFechas$TotalB[i]
    } 
    
  } else { #Trámites
    
    if(BaseFechas$Acciones[i]=="Eliminar"){ #Trámites_Eliminar    
      BaseFechas$Tramites_Eliminar[i] <- BaseFechas$TotalA[i]  
      
    }else if(BaseFechas$Acciones[i]=="Transformar") { #Trámites_Transformar
      BaseFechas$Tramites_Transformar[i] <- BaseFechas$TotalA[i]
      
    } else { # Tramites Acondicionar
      
      if(BaseFechas$Integración[i]=="Publicación ficha informativa"){ #Trámites Acondicionados Publicación Ficha
        BaseFechas$Tramites_Ac_Publicación_Ficha[i] <- BaseFechas$TotalA[i]
        
      } else if(BaseFechas$Integración[i]=="Interfaz gráfica") { #Trámites Acondicionados Interfaz Gráfica
        BaseFechas$Tramites_Ac_Interfaz_Gráfica[i] <- BaseFechas$TotalA[i]
        
      } else if(BaseFechas$Integración[i]=="Uso servicios WEB/API") { #Trámites Acondicionados servicios WEB/API
        BaseFechas$Tramites_Ac_WEB_API[i] <- BaseFechas$TotalA[i]
        
      }  else{
        BaseFechas$Tramites_Ac_Desarrollo_Integrado[i] <- BaseFechas$TotalA[i]
      }
      
    }
    
  }

}

# Gráfico Agregado por fechas y categorías

barFechasDetalle <- 
  plot_ly(BaseFechas, x = ~Fecha, y = ~TotalB  , type = 'bar', name = 'Dominios') %>% #Dominios General
  add_trace(y = ~Dominios_Integrar_SitioWeb, name = 'Dominios a Integrar en Sitio Web Oficial') %>% #Dominios Desagregados
  add_trace(y = ~Dominios_Integrar_a_GovCo, name = 'Dominios a Integrar en GovCo') %>%
  add_trace(y = ~Dominios_Permanece, name = 'Dominios que Permanecen') %>%
  add_trace(y = ~Dominios_Eliminar, name = 'Dominios a Eliminar') %>%
  add_trace(y = ~TotalA, name = 'Trámites') %>% #Trámites Agregados
  add_trace(y = ~Tramites_Ac_Publicación_Ficha, name = 'Trámites Publicación Ficha')%>%
  add_trace(y = ~Tramites_Ac_Interfaz_Gráfica, name = 'Trámites Interfaz Gráfica')%>%
  add_trace(y = ~Tramites_Ac_WEB_API, name = 'Trámites Desarrollo WEB API')%>%
  add_trace(y = ~Tramites_Ac_Desarrollo_Integrado, name = 'Trámites Desarrollo Integrado')%>%
  add_trace(y = ~Tramites_Transformar, name = 'Trámites a Transformar')%>%
  add_trace(y = ~Tramites_Eliminar, name = 'Trámites a Eliminar')%>%
  layout(title = "Acciones por Fecha Desagregado",yaxis = list(title = ''), barmode = 'group')

barFechasDetalle

chart_link11 <- api_create(barFechasDetalle, filename="Acciones por Fecha Desagregado")
chart_link11
chart_link11$web_url



ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)