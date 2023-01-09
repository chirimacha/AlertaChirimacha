##########################
#Codigo Paper AlertaChiri#
##########################

library (dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(tidyr)
library(scatterpie)
library(Cairo)
library(stringr)


setwd("C:/Users/ASUS/Documents/R/Archivos corridas")

#Cargando bases de datos

den<- read.csv("denuncias-02-01-2022.csv", header = TRUE, sep = ",")
distritos<-read.csv("Distritos_AQP.csv", sep=";")
puntos<-read.csv("Puntos_AQP.csv", sep=";")
catchments<-read.csv("data_catchments_complete.csv", sep=",")


##Sacando información solo de denuncias
den<-subset(den, NUMERO_DENUNCIA!="NA")
den<-subset(den, ACTIVIDAD=="DENUNCIA")

#Formateando fecha
den$FECHA <- as.Date(den$FECHA, format="%d/%m/%Y")

#Separando denuncias de arequipa
den_aqp<-subset(den, PROVINCIA=="AREQUIPA")

den_otr<-subset(den, PROVINCIA!="AREQUIPA")


#Creando variable que unifique denuncias por alerta chiri
den_aqp<-mutate(den_aqp, den_alerta= ifelse(MANERA_DENUNCIA=="TELEFONO"|MANERA_DENUNCIA=="WHATSAPP"|MANERA_DENUNCIA=="MESSENGER", 1, 0))

#dejo solo denuncias por Alerta Chiri
den_aqp<-subset(den_aqp, MANERA_DENUNCIA!="ESTABLECIMIENTO")

#creo variable de positividad
den_aqp<-mutate(den_aqp, pos= ifelse(TAXO_INSECTO_DENUNCIA=="Triatoma infestans", 1,
                             ifelse(is.na(TAXO_INSECTO_DENUNCIA), NA,0)))
den_aqp$pos[is.na(den_aqp$pos)] <- 0

#Creando base para hacer mapa de numero de denuncias de alerta chiri por distrito
den_distritos <- den_aqp %>%
  group_by(DISTRITO=den_aqp$DISTRITO) %>%
  summarise(d_alert=sum(den_alerta))

#Creando base para hacer mapa de numero de positivos por distrito
den_pos <- den_aqp %>%
  group_by(DISTRITO=den_aqp$DISTRITO) %>%
  summarise(pos=sum(pos))


#cambiando nombre de variable de distrito en puntos
names(puntos)[names(puntos) == "X"] <- "DISTRITO"

#Agregando puntos gps a den distritos
den_distritos<-merge(den_distritos, puntos, by="DISTRITO", all.x =T, all.y=T)

#Agregando positivas
den_distritos<-merge(den_distritos, den_pos , by="DISTRITO", all.x =T, all.y=T)

#Quitando los distritos lejanos de den_distritos
den_distritos<-den_distritos[!(den_distritos$DISTRITO=="PEDREGAL" |den_distritos$DISTRITO=="LA JOYA" | den_distritos$DISTRITO=="CHARACATO"| den_distritos$DISTRITO=="CHIGUATA" | den_distritos$DISTRITO=="MOLLEBAYA"| den_distritos$DISTRITO=="POCSI" | den_distritos$DISTRITO=="POLOBAYA"| den_distritos$DISTRITO=="QUEQUENA" | den_distritos$DISTRITO=="SABANDIA"| den_distritos$DISTRITO=="VITOR"| den_distritos$DISTRITO=="YARABAMBA"),]

#Quitando los distritos lejanos de "distritos"
distritos<-distritos[!(distritos$ident=="Pedregal" | distritos$ident=="Characato" | distritos$ident=="mollebaya"| distritos$ident=="pocsi" | distritos$ident=="polobaya"| distritos$ident=="quequeña" | distritos$ident=="Sabandia"| distritos$ident=="yarabamba"),]

#Creando variable de negativos
den_distritos$neg<-(den_distritos$d_alert-den_distritos$pos)
names(den_distritos)[names(den_distritos) == "neg"] <- "Others"
names(den_distritos)[names(den_distritos) == "pos"] <- "T. infestans"


########
#Tablas#
########

#Denuncias por provincia

den_otr%>% count(PROVINCIA)

#Denuncias por distrito
table(den_aqp$DISTRITO, den_aqp$MANERA_DENUNCIA, useNA = "ifany")

#Denuncias positivas por distrito y manera de denuncia
table(den_aqp$DISTRITO, den_aqp$MANERA_DENUNCIA, den_aqp$pos)

#Taxonomia
den_tax<-subset(den, MANERA_DENUNCIA!="ESTABLECIMIENTO")
tax<- den_tax %>% count(TAXO_INSECTO_DENUNCIA)
tax

#######
#Mapas#
#######
m<-ggplot(distritos, aes(x = long, y = lat, group= ident)) +
  geom_polygon(color = "white", fill="lightgray")

legend_title <- "Type of report"

p<-m+ geom_scatterpie(aes(x=long, y=lat, group=DISTRITO, r=(0.0010*d_alert)/2), data=den_distritos,
                                cols=c("T. infestans", "Others"), color=NA, alpha=.8) + coord_equal()+
  scale_fill_manual(legend_title, values = c("firebrick", "dodgerblue4"))+
                      scale_y_continuous(name = "Latitude")+
  scale_x_continuous(name = "Longitude")+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        panel.background=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.title = element_text(size=13), legend.text = element_text(size=13),
        legend.key.size = unit(0.8, 'cm'),
        legend.position = c(.88, .86),
        legend.background=element_blank())

den_distritos$radius2<- ((sqrt(den_distritos$d_alert)/sqrt(100))/30)

m2<-p+geom_scatterpie_legend(den_distritos$radius2, x=-71.69, y=-16.48, n=4,labeller=function(x) x/0.0010*2)
m2

ggsave(m2, file = "map1.png", width = 8, height = 6.5, type = "cairo-png")

####################
#Gráfico del radio#
####################

#Selecciono solo un catchment a graficar (c2pau)

c2pau_pun<-subset(catchments, catchment_name=="C2PAU")
c2pau_pol<-read.csv("C2PAU.csv", sep=";")
pos<-subset(c2pau_pun, UNICODE=="1.13.51.1683")
names(pos)[names(pos) == "UNICODE"] <- "ID"
pos = subset(pos, select = c(ID,LONGITUDE,LATITUDE))

#Haciendo función para dibujar radio
make_circles <- function(centers, radius, nPoints = 100){
  # centers: the data frame of centers with ID
  # radius: radius measured in kilometer
  meanLat <- mean(centers$LATITUDE)
  # length per longitude changes with lattitude, so need correction
  radiusLon <- radius /111 / cos(meanLat/57.3) 
  radiusLat <- radius / 111
  circleDF <- data.frame(ID = rep(centers$ID, each = nPoints))
  angle <- seq(0,2*pi,length.out = nPoints)
  
  circleDF$lon <- unlist(lapply(centers$LONGITUDE, function(x) x + radiusLon * cos(angle)))
  circleDF$lat <- unlist(lapply(centers$LATITUDE, function(x) x + radiusLat * sin(angle)))
  return(circleDF)
}
myCircles <- make_circles(pos, 0.2)


radio<- ggplot() + theme_void() +
  geom_point(data=c2pau_pun, 
             aes(x=LONGITUDE, y=LATITUDE, fill="black"), size = 2.4, shape=19)+
  geom_point(data=pos, 
             aes(x=LONGITUDE, y=LATITUDE, colour="red"),  size = 3, shape=16)
  
two<-radio+geom_polygon(data = myCircles, aes(lon, lat, group = ID, fill="blue"), color = "blue", alpha = 0.2)+
  scale_fill_manual(name="", values = c("blue"="blue"),
    labels = c("Radius"))+
  scale_color_manual(values = c("red"="red"), name="", 
    labels = c("Positive house"))+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        panel.background=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.title = element_text(size=15), legend.text = element_text(size=15),
        legend.key.size = unit(0.5, 'cm'),
        legend.position = c(.12, .86),
        legend.background=element_blank())

##############################################
#Information for Rapid and efficient response#
##############################################

ins_old<- read.csv("inspections-23-12-22.csv", header = TRUE, sep = ";")
roc_old<- read.csv("rociado-20-12-22.csv", header = TRUE, sep = ";")
ins_new<- read.csv("inspecciones-appnuevo-23-12-2022.csv", sep=";", header = TRUE)
pos<-read.csv("Positivas.csv", sep=";", header = TRUE)


#Formateando fecha#
ins_old$FECHA <- as.Date(ins_old$FECHA, format="%Y/%m/%d")
ins_new$fecha <- as.Date(ins_new$fecha, format="%d/%m/%Y")
roc_old$ROC_FECHA<-as.Date(roc_old$ROC_FECHA, format="%Y/%m/%d")
pos$fecha_rep <- as.Date(with(pos, paste(rep_fec_dia, rep_fec_mes, rep_fec_ano,sep="-")), "%d-%m-%Y")
pos$fecha_ins <- as.Date(with(pos, paste(insp_fec_dia, insp_fec_mes, insp_fec_ano,sep="-")), "%d-%m-%Y")
pos$fecha_roc <- as.Date(with(pos, paste(roc_fec_dia, roc_fec_mes, roc_fec_ano,sep="-")), "%d-%m-%Y")


#Separando desde octubre 2020, fecha en la que iniciamos alerta chirimacha 
insp<-ins_old[ins_old$FECHA >= "2020-10-1" & ins_old$FECHA <= "2022-12-31", ]
insp_new2<-ins_new[ins_new$fecha >= "2020-10-1" & ins_new$fecha <= "2022-12-31", ]
roc_old2<-roc_old[roc_old$ROC_FECHA >= "2020-10-1" & roc_old$ROC_FECHA <= "2022-12-31", ]

#Dejando solo los ingresos relacionados con inmune
insp_old2<-insp[(insp$USER_NAME=="I1" |insp$USER_NAME=="I2" |insp$USER_NAME=="I3"|insp$USER_NAME=="I4" |insp$USER_NAME=="I5" |insp$USER_NAME=="I6" |insp$USER_NAME=="I7" |insp$USER_NAME=="I8" |insp$USER_NAME=="I9" |insp$USER_NAME=="MAR"|insp$USER_NAME=="MIA" |insp$USER_NAME=="MLS" |insp$USER_NAME=="OCA" |insp$USER_NAME=="RQA" |insp$USER_NAME=="ZAC" |insp$USER_NAME=="S1"|insp$USER_NAME=="S2"),]

#Quitando rociados de pedregal 
roc_old2<-roc_old2[roc_old2$USU_MICRORED !="20",]

#Quitando rociados que estaban pendientes del 2017 Y se hicieron el 18/10/2021
roc_old2<-roc_old2[which(roc_old2$ROC_FECHA!="2021-10-18"),]


##Contando número de inspecciones en general###

#Sin Minsa
insp_old2%>% count(STATUS_INSPECCION)
insp_new2%>% count(resultado_de_la_inspeccion)

#Con Minsa
insp%>% count(STATUS_INSPECCION)

##Contando número de inspecciones debidas a denuncia (Falta agregar del app nuevo, pero la base que me dieron no tiene columna de inspecciones por denuncia o radio, manualmente por denuncia/radio son 40)
length(intersect(which(insp_old2['STATUS_INSPECCION']=="inspeccion"), which(insp_old2['DEN_ID_CUSTOM']!= "NA")))

##Contando inspecciones debidas a radio
length(intersect(which(insp_old2['STATUS_INSPECCION']=="inspeccion"), which(insp_old2['INSP_POR_RADIO']!="",)))

#Contando entrevistas
insp_old2%>% count(STATUS_INSPECCION=="entrevista")
insp_new2%>% count(resultado_de_la_inspeccion=="interviewed")

#Contando rociado (Falta base de app nuevo, pero no la tengo, aunque se que se rocaron 4 casas)
roc_old2%>% count(ROC_TRATAMIENTO_RESIDUAL=="T")


#Elimino de base positivas dos viviendas encontradas positivas en 2017 y rociadas en 2020
pos<-pos[which(pos$unicode!="1.13.96.627" & pos$unicode!="1.13.87.224A"),]

#Elimino vivienda reportada durante pandemia
pos<-pos[which(pos$unicode!="1.18.9.143"),]

#Days from report to inspect
pos$rep_ins<-pos$fecha_ins-pos$fecha_rep
pos$rep_ins<-as.numeric(pos$rep_ins)

#Days from inspection to spray
##Tomo solo viviendas que reportaron 
#pos_rep<-pos[(pos$fuente.de.vivienda== "report"),]
pos$ins_roc<-pos$fecha_roc-pos$fecha_ins
pos$ins_roc<-as.numeric(pos$ins_roc)

#
pos_alerta<-pos[(pos$fuente_de_FOCO== "reporte_alertachiri" | pos$fuente_de_FOCO== "radio_inspection"),]
pos_convencional<-pos[pos$fuente_de_FOCO== "reporte_convencional",]
pos_acti<-pos[pos$fuente_de_FOCO== "inspecciones_activas",]


#histograma todas las positivas
hist(pos$rep_ins, breaks =15 , main = "Report to inspection", xlab= "Days")
hist(pos$ins_roc, breaks =15 , main = "inspection to report", xlab= "Days")

#histogramas separados por fuente de foco
par(mfrow = c(2, 2))

hist(pos_alerta$rep_ins, breaks =8 , main = "Report to inspection / Alerta Chiri", xlab= "Days")
hist(pos_alerta$ins_roc, breaks =10 , main = "Inspection and Sprayed / Alerta Chiri", xlab= "Days")
hist(pos_convencional$rep_ins, breaks =10, main = "Report to inspection / conventional", xlab= "Days")
hist(pos_convencional$ins_roc, breaks =8 , main = "Inspection and Sprayed / conventional", xlab= "Days")



rep_ins<-data.frame(table(pos_alerta$rep_ins))
ins_roc<-data.frame(table(pos_alerta$ins_roc))

                    