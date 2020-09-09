rm(list=ls())
setwd("~")

library(tidyverse)

setwd("/home/debby/Documents/DataScience/project/")
gasRow <- read.csv("agostot2020.csv",stringsAsFactors = TRUE, strip.white = TRUE)

gas <- NULL
gas <- gasRow[c("Date2","Textbox70","Product1","Textbox42","Price1","Textbox44","Pump1","Cashier1")]


#Filtrar gas y disel
gas <- gas[c(gas$Product1=="GAS93RM"| 
               gas$Product1=="GAS95RM" | 
               gas$Product1=="GAS97RM" | 
               gas$Product1=="DIXTRA"),] 


#Marcar dias de la semana
dia.semana = c()
dia.asignado = 6 # 1-Lunes 2-Martes 3-Miercoles etc...
dia.actual = gas[1,"Date2"]

for (i in 1:nrow(gas)) {
  if (dia.actual != gas[i,"Date2"]) {
    dia.actual = gas[i,"Date2"]
    if (dia.asignado == 7) {
      dia.asignado = 1
    } else {
      dia.asignado = dia.asignado + 1
    }
  } 
  
  dia.semana[i] = dia.asignado  
  
}

gas <- cbind(gas,dia.semana)


#Marcar fines de semana
es.fin.de = c()

for (i in 1:nrow(gas)) {
  if (gas[i,"dia.semana"] == 6 | gas[i,"dia.semana"] == 7) {
    es.fin.de[i] = T
  } else {
    es.fin.de[i] = F
  }
}

gas <- cbind(gas,es.fin.de)


#Renombrar columnas
colnames(gas) <- c("Fecha",
                   "Hora",
                   "Producto",
                   "Litros",
                   "PrecioUni",
                   "PrecioTot",
                   "Bomba",
                   "Cobrador",
                   "Dia.semana",
                   "Fin.de")

#Formateo de precios
gas$PrecioTot = gasRow[c(gasRow$Product1=="GAS93RM"|
                         gasRow$Product1=="GAS95RM" |
                         gasRow$Product1=="GAS97RM" |
                         gasRow$Product1=="DIXTRA"),c("Textbox44")]
gas$PrecioTot = gsub(",","", gas$PrecioTot)
gas$PrecioTot = as.numeric(gas$PrecioTot)
gas$PrecioTot = gsub("\..","", gas$PrecioTot)##OJOAQUI

sum(gas$PrecioTot)

#Marcar numero de semana

dia.de.inicio = gas[1,"Dia.semana"]
dia.actual = gas[1,"Fecha"]
numero.asigna.semana = 1
Numero.semana = c()

for (i in 1:nrow(gas)) {
  if (gas[i,"Dia.semana"] == dia.de.inicio & dia.actual != gas[i,"Fecha"]) {
    numero.asigna.semana = numero.asigna.semana + 1
    dia.actual = gas[i,"Fecha"]
  }
  Numero.semana[i] = numero.asigna.semana
}

gas <- cbind(gas, Numero.semana)


# Sub dataframes

gas$PrecioTot
gas[gas$Producto=="GAS97RM" & gas$Numero.semana==4,]
summarise(gas, venta = sum(gas$PrecioTot))
nrow(gas[gas$Producto=="GAS93RM",])


#Creacion de grafico ventas
gr <- ggplot(gas, aes(x=Producto, y=Bomba)) +
  geom_point()
gr

gr <- ggplot(gas, aes(x=Producto, y=Bomba)) +
  geom_bar(stat="identity", fill="#41b6c4") +
  geom_text(aes(label=Producto), vjust=-0.3, size=3) +
  facet_wrap(~PrecioUni) +
  theme_bw()
ggsave(paste(dir2, "grafico.png", sep="/"), plot=gr, width=12, height=12)
gr

