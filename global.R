library(shiny)
library(leaflet)
library(plyr)
library(dplyr)
library(grid)
library(vcd)
library(plotly)
library(ggplot2)
library(reshape)
library(tidyr)
library(rpivotTable)


#library(ggplot2movies)



#For PC
#setwd("C:\\Users\\aponce\\Dropbox\\JANO\\2016\\Conabio\\Github\\shiny_maiz\\")

#For Mac
#setwd("~/Dropbox/JANO/2016/Conabio/Github/shiny_Corredor/")
dir()
TableP <- read.csv("Agrobiodiversidad Concentrado_FINAL.csv", header = T, sep = ",")
head(TableP)
str(TableP)
str(TableP$Latitud)

TableP$CodMunicip <- sprintf("%05d", TableP$CodMunicip)

levels(TableP$Estado)
levels(TableP$Región)
levels(TableP$Municipio)
levels(TableP$Localidad)
head(TableP)
names(TableP)
head(TableP)
summary(TableP)


TableP1 <- TableP %>%
  mutate(Municipio = revalue(Municipio,c("maravilla Tenejapa" = "Maravilla Tenejapa"))) %>%
  mutate(Val1 = rep(1, nrow(TableP))) %>% #Create a new column
  filter(Estado != "") %>%
  filter(Sistema != "") %>%
  #mutate(Sistema = revalue(Sistema,c("agroforestal" = "Agroforestal"))) %>%
  mutate(Sistema = revalue(Sistema,c("Agroforesteria con café" = "Agroforestería con café"))) %>%
  #mutate(Sistema = revalue(Sistema,c("monocultivo" = "Monocultivo"))) %>%
  mutate(Región = revalue(Región,c("region costa " = "Costa"))) %>%
  mutate(Región = revalue(Región,c("region loxicha" = "Loxicha"))) %>%
  mutate(Región = revalue(Región,c("region mixteca" = "Mixteca"))) %>%
  mutate(Región = revalue(Región,c("region sierra norte" = "Sierra Norte"))) %>%
  mutate(Región = revalue(Región,c("sierra norte " = "Sierra Norte"))) %>%
  mutate(Región = revalue(Región,c("sierra sur" = "Sierra Sur"))) %>%
  mutate(Región = revalue(Región,c("Sierra sur" = "Sierra Sur"))) %>%
  mutate(Localidad = revalue(Localidad,c("Bella Ilusion" = "Bella Ilusión"))) %>%
  mutate(Localidad = revalue(Localidad,c("Niños Heroes" = "Niños Héroes"))) %>%
  mutate(Localidad = revalue(Localidad,c("cerro cantor" = "Cerro cantor"))) %>%
  mutate(Localidad = revalue(Localidad,c("chilapa loxicha" = "Chilapa"))) %>%
  mutate(Localidad = revalue(Localidad,c("Chipa" = "Chilapa"))) %>%
  mutate(Localidad = revalue(Localidad,c("Chilpa" = "Chilapa"))) %>%
  mutate(Localidad = revalue(Localidad,c("sanjose de las flores" = "San José de las Flores"))) %>%
  mutate(Localidad = revalue(Localidad,c("santa maria zoogochi" = "Santa María Zoogochi"))) %>%
  #filter(!is.na(Latitud)) %>% #eliminate the NA's
  dplyr::rename(lat = Latitud) %>% #Change the name of the column
  dplyr::rename(lng = Longitud) %>% #Change the name of the column
  filter(!is.na(lat)) %>% #eliminate the NA's
  filter(!is.na(lng)) %>% #eliminate the NA's
  mutate(NomSci = paste(Género, Especie, sep = " ")) %>% # crear una nueva columna
  distinct() %>% #Remove duplicate rows
  droplevels() #Eliminar los niveles

head(TableP1)  
dim(TableP1)
dim(TableP)

names(TableP1)
head(TableP1)

#attach(TableP1)
#TablePLL <- aggregate(Val1~NomSci + Especie + Género + Nombre_común, FUN = mean)
#detach(TableP1)
#head(TablePLL)
#write.table(TablePLL, file = "Data_Karla.txt", sep = "\t")



TableL <- TableP1

TableL1 <- TableP1

head(TableL1)

TableL101 <- TableL1[,c(15,4,3, 2, 7, 13, 8)]
head(TableL101)

#rpivotTable(TableL101)


#TableL101$NomSci
#TableL101 <- TableL101[order(TableL101$NomSci),]
#head(TableL101)

#
head(TableL1)
names(TableP1)
TableL2 <- TableP1[,c(2,3,5,13,15,14)]
TableL2 <- na.omit(TableL2)
dim(TableL2)
head(TableL2[1:4,])
TableL2 <- list(var = TableL2[,c(1,2,4)], valor = TableL2[,c(5,6)])
head(TableL2)
names(TableL2$var)
head(TableL2$var)
summary(TableL2)
dim(TableL2$var)
TableL3 <- data.frame(TableL2$var, TableL2$valor)
names(TableL3)[5] <- c("value") 
TableL3 <- TableL3 %>%
  mutate(Estado = revalue(Estado,c("Oaxaca" = "Oax"))) %>%
  mutate(Estado = revalue(Estado,c("Chiapas" = "Chis"))) %>%
  mutate(Sistema = revalue(Sistema,c("Agroforestería con café" = "Agrof_café"))) %>%
  mutate(Sistema = revalue(Sistema,c("Bosque Mesofilo de Montaña" = "Bosq_Mesófilo"))) %>%
  mutate(Sistema = revalue(Sistema,c("Ganadería Silvopastoril" = "Gan_Silvopast"))) %>%
  mutate(Sistema = revalue(Sistema,c("Milpa Sustentable" = "Mil_Sustentable"))) %>%
  mutate(Sistema = revalue(Sistema,c("Recolección y Cacería" = "Recol_Cacería")))
  
levels(TableL3$Sistema) 
#TableL3$Estado <- abbreviate(TableL3$Estado, minlength = 3)
#TableL3$Sistema <- abbreviate(TableL3$Sistema, minlength = 8)
#TableL3$Municipio <- abbreviate(TableL3$Municipio, minlength = 10)
head(TableL3)

head(TableL3)

#Tabla4 <- TableL3 %>%
#  spread(NomSci, Variable)

TableL4 <- cast(TableL3, Estado + Sistema + Municipio ~ NomSci, fun.aggregate = sum)

TableL5 <- as.data.frame(TableL4[,-c(1:3)])
head(TableL5)
dim(TableL4)
dim(TableL4)
head(TableL4)[1:5]
levels(TableL4$Estado)

Vector1 <- paste(TableL4$Estado, TableL4$Sistema, TableL4$Municipio, sep = ".")
TableL4 <- data.frame(Vector1, TableL4)

#str(TableL4)
#TableL5 <- aggregate(TableL4[,-c(1:3)],list(TableL4$Sistema), FUN = sum, na.rm = T)
#head(TableL5)[1]

#Rarefraction
RarefraccionCC <- function(Tabla,factor){
  require(vegan)
  Tabla1 <- data.frame(Tabla, row.names = factor)
  raremax <- min(rowSums(decostand(Tabla1,"pa")))
  col1 <- seq(1:nrow(Tabla1)) #Para poner color a las lineas
  lty1 <- c("solid","dashed","longdash","dotdash")
  rarecurve(Tabla1, sample = raremax, col = "black", lty = lty1, cex = 1)
  #Para calcular el numero de especies de acuerdo a rarefraccion
  UUU <- rarefy(Tabla1, raremax)
  print(UUU)
}

#RarefraccionCC(TableL5[,-c(1)],TableL5[,1])

#Renyi

RenyiCC <- function(Tabla, factor){
  require(vegan) #Paquete para la funcion "renyi"
  require(ggplot2)#Paquete para hacer la funcion "qplot"
  require(reshape)#Paquete para la funcion "melt"
  Tabla <- data.frame(Tabla, row.names = factor)
  mod <- renyi(Tabla)
  vec <- seq(1:11)
  mod1 <- data.frame(vec,t(mod))
  mod2 <- melt(mod1, id = c("vec"))
  mod2
  #mod2$variable <- as.numeric(mod2$variable)
  orange <- qplot(vec, value, data = mod2, colour = variable, geom = 	"line") + theme_bw() + theme(legend.text = element_text(size = 14))
  orange + scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("0","0.25","0.5","1","2","4","8","16","32","64","Inf"))
}

