#titulo. El 10% de la UNAM#
#Autor: Israel Garcia Solares#
#idioma: Espa?ol#
#License: CC4#
install.packages("ineq")
library(ineq)
library(tidyverse)
library(plyr)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
####Profesores Carrera####
carrera<-read_csv("carrera.csv") 
#descargado de http://www.transparencia.unam.mx/obligaciones/consulta/remuneracion-profesores
funcionarios<-read_csv("funcionarios.csv")
funcionariosextra<-read_csv("funcionariosextra.csv")
funcionariosextra<-subset(funcionariosextra, !str_detect(funcionariosextra$denominacion, "OTROS(\\s)INGRESOS"))
#descargados de http://www.transparencia.unam.mx/obligaciones/consulta/remuneracion-personal 
sni<-read_csv("sni.csv")
sni$`Apellido Materno`[which(sni$`Apellido Materno`=="sin apellido Materno")]<-NA
#Padron de 2018 de la UNAM descargado de https://datos.gob.mx/busca/dataset/sistema-nacional-de-investigadores
sni2<-read_csv("sni2019.csv")
#descargado de https://www.conacyt.gob.mx/images/SNI/2019/RESULTADOS_SNI_CONVOCATORIA_2019_INGRESO_O_PERMANENCIA.pdf . Esta version continene los miembros que solicitaron renovaci?n para 2020#
sniresto<-read_csv("sni2018.csv")
#Padron de 2018 general, con el fin de identificar los investigadores que migraron a la UNAM, decargado de https://datos.gob.mx/busca/dataset/sistema-nacional-de-investigadores

colnames(carrera)<-c("unidad", "nombre", "apellido1", "apellido2", "tipo", "bruta", "neta", "estimulos", "total")
carrera$id<-rownames(carrera)
carrera$nombre1<-str_extract(carrera$nombre, "^(\\w+)")
funcionarios$bruta<-str_replace(funcionarios$bruta, "[$]", "")
funcionarios$bruta<-str_replace(funcionarios$bruta, "(?<=(\\d))[,](?=(\\d))", "")
funcionarios$bruta<-as.numeric(as.character(funcionarios$bruta))
funcionariosextra$bruta<-str_replace(funcionariosextra$bruta, "[$]", "")
funcionariosextra$bruta<-str_replace(funcionariosextra$bruta, "(?<=(\\d))[,](?=(\\d))", "")
funcionariosextra$bruta<-as.numeric(as.character(funcionariosextra$bruta))
funcionariosextra$brutas<-ifelse(funcionariosextra$periodicidad=="ANUAL", funcionariosextra$bruta/12, 
                                 ifelse(funcionariosextra$periodicidad=="SEMESTRAL", funcionariosextra$bruta/6,  
                                        funcionariosextra$bruta))
extraf<-ddply(funcionariosextra, 
              c("id"), 
              summarize, 
              estimulos=sum(brutas))
funcionarios<-left_join(funcionarios, extraf, by="id")
funcionarios$administrativo<-funcionarios$bruta+funcionarios$estimulos
funcionarios$nombre1<-str_extract(funcionarios$nombre, "^(\\w+)")
#adicionar a los profesores de carrera
carrera<-left_join(carrera, 
                   subset(funcionarios, select=c("apellido1", "apellido2", "nombre1", "administrativo")), by=c("apellido1", "apellido2", "nombre1"))
#ahora adicionar los est?mulos del SNI
colnames(sni)<-c("apellido1", "apellido2", "nombre", "nivel", "institucion", "area")
sni2$nombre<-ifelse(!is.na(str_extract(lag(sni2$text), "^(\\d+)$")), sni2$text, NA)
sni2$nivel<-ifelse(!is.na(str_extract(lead(sni2$text), "^(\\d+)$")), sni2$text, NA)
sni2<-fill(sni2, nombre, .direction="down")
sni2$nivel<-ifelse(!is.na(str_extract(sni2$nivel, "CANDIDAT")), "C", 
                   ifelse(!is.na(str_extract(sni2$nivel, "(\\b)I$")), 1, 
                          ifelse(!is.na(str_extract(sni2$nivel, "(\\b)II$")), 2, 
                                 ifelse(!is.na(str_extract(sni2$nivel, "(\\b)III$")), 3, NA))))
sni3<-subset(sni2, !is.na(sni2$nivel), select=c("text","nombre", "nivel"))
sni3<-separate(sni3, nombre, into=c("apellidos", "nombre"), sep=",")
sni3$nombre<-trimws(sni3$nombre, which = "both")
sni3$apellidos<-trimws(sni3$apellidos, which = "both")
sni3$name<-paste(sni3$apellidos, ", ", str_extract(sni3$nombre, "^(\\w+)"), sep="")
sniresto<-rbind(subset(sni3, select = c("name", "nivel")),subset(sniresto, select = c("name", "nivel")))
sniresto<-distinct(sniresto, name, .keep_all = T)
sni$name<-paste(sni$apellido1, ifelse(!is.na(sni$apellido2), paste(" ", sni$apellido2, ", ", sep=""), ", "), 
                str_extract(sni$nombre, "^(\\w+)"), sep="")
carrera$name<-paste(carrera$apellido1, ifelse(!is.na(carrera$apellido2), paste(" ", carrera$apellido2, ", ", sep=""), ", "), 
                    str_extract(carrera$nombre, "^(\\w+)"), sep="")
carrera$nombramiento<-str_replace_all(carrera$tipo, "^(\\w+)(\\s+)", "")
carrera$nombr<-str_to_title(str_extract(carrera$nombramiento, "EMERITO"))
carrera$nombr<-ifelse(is.na(carrera$nombr), 
                      str_to_title(str_extract(carrera$nombramiento, "EMERITO|TECNICO|PROFESOR|INVESTIGADOR")), 
                      carrera$nombr)
carrera2<-left_join(subset(carrera, select=c("id","name","unidad", "tipo", "bruta", "neta", "estimulos","administrativo","apellido1", "apellido2", "nombr")),
                    subset(sni, select=c("apellido1", "apellido2", "nivel")), by=c("apellido1", "apellido2"))
carrera2<-distinct(carrera2, id, .keep_all = T)
carrera2a<-subset(carrera2, !is.na(carrera2$nivel))
carrera2b<-subset(carrera2, is.na(carrera2$nivel))
carrera2c<-left_join(subset(carrera2b, select=-c(nivel)), sniresto , by="name")
carrerat<-rbind(carrera2a, carrera2c)
carreratest<-carrerat[which(!is.na(carrerat$nivel)),]
carrerat$sni<-ifelse(carrerat$nivel=="C", 8186.96,
                     ifelse(carrerat$nivel=="1", 14327.18,
                            ifelse(carrerat$nivel=="2", 18420.66,
                                   ifelse(carrerat$nivel=="3", 30701.10,0))))
carrerat$estimulos[is.na(carrerat$estimulos)]<-0
carrerat$sni[is.na(carrerat$sni)]<-0
carrerat$administrativo[is.na(carrerat$administrativo)]<-0
carrerat$total<-carrerat$bruta+carrerat$estimulos+carrerat$sni+carrerat$administrativo+1255
carrerat$totalneto<-carrerat$neta+carrerat$estimulos+carrerat$sni
carrerat$unidad<-str_to_title(carrerat$unidad)
carrerat<-carrerat[order(carrerat$total),]
carrerat$orden<-1:12882
carrerag<-pivot_longer(subset(carrerat, select=c("name", "orden", "bruta", "estimulos", "sni","administrativo")), c("bruta", "estimulos", "sni", "administrativo"), names_to = "tipo", values_to="remuneracion")
carrerag<-subset(carrerag, carrerag$remuneracion!=0)
carrerat$decil<-with(carrerat, cut.default(floor(total), 
                                           breaks=quantile(floor(total), probs=seq(0,1, by=0.1)), 
                                           labels=c(1:10), right=T, include.lowest=T))
carreraX<-carrerat[which(carrerat$decil==10),]
carreraX$centil<-with(carreraX, cut.default(floor(total), 
                                            breaks=quantile(floor(total), probs=seq(0,1, by=0.1)), 
                                            labels=c(91:100), right=T, include.lowest=T))
carreragX<-pivot_longer(subset(carreraX, select=c("name", "orden", "bruta", "estimulos", "sni", "administrativo")), c("bruta", "estimulos", "sni", "administrativo"), names_to = "tipo", values_to="remuneracion")
ggplot(data=carrerag, aes(x=orden, y=remuneracion, fill=tipo))+geom_col()
ggplot(data=carreragX, aes(x=orden, y=remuneracion, fill=tipo))+geom_col()


####Profesores de Asginatura y Ayudantes####
#asumimos una distribuci?n normal de horas, usando la media de acuerdo al anuario estadistico DGAPA 2020 https://www.planeacion.unam.mx/Agenda/2020/disco/#
set.seed(2021)
asignaturaa<-rnorm(n=22011, mean=12.25898414, sd=3.086328048)
asignaturaa<-as.data.frame(asignaturaa)
colnames(asignaturaa)<-"horas"
asignaturaa$nombr<-"Asignatura A"
asignaturaa$tarifa<-395.38
asignaturab<-rnorm(n=2410, mean=20.65062241, sd=5.883540802)
asignaturab<-as.data.frame(asignaturab)
colnames(asignaturab)<-"horas"
asignaturab$nombr<-"Asignatura B"
asignaturab$tarifa<-432.3
ayudante<-rnorm(n=4130, mean=10.6842615, sd=2.5614205)
ayudante<-as.data.frame(ayudante)
colnames(ayudante)<-"horas"
ayudante$nombr<-"Ayudante"
ayudante$tarifa<-321.94
precarios<-rbind(asignaturaa, asignaturab, ayudante)
precarios$horas<-ifelse(precarios$horas<2.5, 2.5, precarios$horas)
precarios$horas<-ifelse((precarios$horas>15)&(precarios$nombr!="Asignatura B"), 15, precarios$horas)
precarios$bruta<-precarios$horas*precarios$tarifa
precarios$despensa<-1255
precarios$asistencia<-precarios$bruta/12
precarios$extra<-ifelse(precarios$horas>=15, precarios$horas*2.5, 0)

#para agregar la desigualdad basada en el PEPASIG, vamos a suponer una distribuci?n de de antiguedad que replique la distribuci?n de horas
#y el porcentaje hasta 20 a?os de antiguedad del Anuario Estad?stico Dgapa 2020 ## 
#Es una estimaci?n generosa, en tanto la distribuci?n de antiguedad acad?mica favorece a los profesores de carrera en general#
precarios<-precarios[order(precarios$horas),]
precarios$orden<-1:28551
precarios$ant<-ifelse(precarios$orden<7283, "0-2", 
                      ifelse(precarios$orden>=7283&precarios$orden<12862, "3-5", 
                             ifelse(precarios$orden>=12862&precarios$orden<16969, "6-8", 
                                    ifelse(precarios$orden>=16969&precarios$orden<20476, "9-11", 
                                           ifelse(precarios$orden>=20476&precarios$orden<23496, "12-14", 
                                                  ifelse(precarios$orden>=23496&precarios$orden<26097, "15-17", 
                                                         ifelse(precarios$orden>=26097, "18-20", 
                                                                NA)))))))
#asignaremos pepasig correspondiente a licenciatura, aunque los profesores de asignatura ocupan tambi?n alrededor del 50% de las clases de posgrado#
#liga aqui https://dgapa.unam.mx/index.php/estimulos/pepasig#
precarios$pepasig<-ifelse(precarios$ant=="0-2", 0,
                          ifelse(precarios$ant=="3-5", 619,
                                 ifelse(precarios$ant=="6-8", 1235,
                                        ifelse(precarios$ant=="9-11", 1974,
                                               ifelse(precarios$ant=="12-14", 2802,
                                                      ifelse(precarios$ant=="15-17", 3502,
                                                             ifelse(precarios$ant=="18-20", 3911,
                                                                    NA)))))))
#agregar todos los estimulos
precarios$estimulos<-precarios$despensa+precarios$asistencia+precarios$extra+precarios$pepasig
precarios$orden<-NULL
precarios$total<-precarios$bruta+precarios$estimulos
precarios$name<-"Anonime"
precarios$sni<-0
precarios$administrativo<-0

####La distribucion final####
todos<-rbind(
  subset(carrerat, select=c("name" , "nombr", "bruta", "estimulos", "sni", "administrativo", "total")), 
  subset(precarios, select=c("name" , "nombr", "bruta", "estimulos", "sni", "administrativo", "total"))
)
todos<-todos[order(todos$total),]
todos$orden<-1:41433
todos$decil<-with(todos, cut.default(floor(total), 
                                     breaks=quantile(floor(total), probs=seq(0,1, by=0.1)), 
                                     labels=c(1:10), right=T, include.lowest=T))
todos$nombramiento<-ifelse(todos$administrativo!=0, "Funcionario", todos$nombr)
todosg<-pivot_longer(subset(todos, select=c("name", "orden", "bruta", "nombr","estimulos", "sni", "administrativo")), c("bruta", "estimulos", "sni", "administrativo"), names_to = "tipo", values_to="remuneracion")
todosg<-subset(todosg, todosg$remuneracion!=0)
unoporcieng<-subset(todosg, todosg$orden>41018.67)
unoporcien<-subset(todos, todos$orden>41018.67)
tablaingreso<-ddply(todos, 
                    c("decil"), 
                    summarize, 
                    ingreso=sum(total))
Gini(todos$total)
sum(unoporcien$total)-sum(todos$total[which(todos$orden<12776)])
mean(todos$total[which(todos$orden>41423)])
mean(todos$total[which(todos$orden<11)])
mean(todos$total[which(todos$orden>41423)])/mean(todos$total[which(todos$orden<11)])
sobresalario<-todos[which(todos$total>111990),]
bajosalario<-todos[which(todos$total<(2.03577*3746)),]
sobresalario$sobre<-sobresalario$total-111990
sum(sobresalario$sobre)
bajosalario$bajo<-(2.0357*3746)-bajosalario$total
sum(bajosalario$bajo)
todos$proyectado<-ifelse(todos$total<(2.036*3746), 2.036*3746, 
                         ifelse(todos$total>111990, 111990, todos$total))
Gini(todos$proyectado)/Gini(todos$total)

todosg$tipo<-str_to_title(todosg$tipo)
ggplot(data=todosg, aes(x=orden, y=remuneracion, fill=tipo))+geom_col()+
  scale_y_continuous(breaks=c(0,5000,10000, 50000, 100000, 150000, 200000, 250000))+
  labs(fill = "Tipo de ingreso",y="Remuneraci?n total" , title="Gr?fico 3a. Remuneraci?n de profesores de menor a mayor")
ggplot(data=todos, aes(x=orden, y=total, fill=nombramiento))+geom_col()+ 
  scale_y_continuous(breaks=c(0,5000,10000, 50000, 100000, 150000, 200000, 250000))+
  labs(fill = "Nombramiento",y="Remuneraci?n total" , title="Gr?fico 3b. Remuneraci?n de profesores de menor a mayor")
  
ggplot(data=todos, aes(x=orden, y=proyectado, fill=nombramiento))+geom_col()+ 
  scale_y_continuous(breaks=c(0,5000,10000, 50000, 100000, 150000, 200000, 250000))+
  labs(fill = "Nombramiento",y="Remuneraci?n total" , title="Gr?fico 3c. Remuneraci?n de profesores de menor a mayor con limites")

write.csv(tablaingreso, "decilesingreso.csv", row.names = F)
write.csv(todos, "todes.csv", row.names = F)
