#### preparando la base####
#ANTES DE COMENZAR RECUERDA CORREGIR EN EL EXCEL EDAD DE HIJOS CON PALABRAS Y 
#REVISAR QUE LOS FILTROS DE LAS PREGUNTAS ESTEN OK#

#abrir paquetes#
library(tidyr)
library(readxl)
library(magrittr)
library(dplyr)
library("writexl") 
library(flextable)
library(reshape2)
library(ggplot2)

#leer la base de datos#
Encuesta <- read_excel("DATOS/BASE1-Encuesta.xlsx")

#Guardamos nombres originales en vector#
nombresorig <- names(Encuesta)

#Eliminamos las 5 primeras filas por no ser validas#
Encuesta <- Encuesta[-c(1:5),]

#Renombramos la variables#
names(Encuesta)[c(12:14, 16:26, 36:42)]<- c("Sexo", "Edad", "Etnia", "Educación",
"JefeHogar", "Hijos", "NHijos", "EHijos",
"Cuida", "CuidaHoras","CuidaInterrumpe",
"Antiguedad", "Jefe", "NivelJefe", 
"RangoSalario", "Negocia","PoliticaGenero",
"AcosoHostigamiento", "ObstaculoMujeres",
"País", "RolAP")

# Nuevos nombres de preguntas 1 a 9
names(Encuesta)[c(3:11)]<- paste0("P1_", c(1:9))

# Nombres de atributos
t11 <- unlist(strsplit(nombresorig[3], split='[', fixed=TRUE))[2] %>%
substring(., 1, nchar(.)-1)
t12 <- unlist(strsplit(nombresorig[4], split='[', fixed=TRUE))[2] %>%
substring(., 1, nchar(.)-1)
t13 <- unlist(strsplit(nombresorig[5], split='[', fixed=TRUE))[2] %>%
substring(., 1, nchar(.)-1)
t14 <- unlist(strsplit(nombresorig[6], split='[', fixed=TRUE))[2] %>%
substring(., 1, nchar(.)-1)
t15 <- unlist(strsplit(nombresorig[7], split='[', fixed=TRUE))[2] %>%
substring(., 1, nchar(.)-1)
t16 <- unlist(strsplit(nombresorig[8], split='[', fixed=TRUE))[2] %>%
substring(., 1, nchar(.)-1)
t17 <- unlist(strsplit(nombresorig[9], split='[', fixed=TRUE))[2] %>%
substring(., 1, nchar(.)-1)
t18 <- unlist(strsplit(nombresorig[10], split='[', fixed=TRUE))[2] %>%
substring(., 1, nchar(.)-1)
t19 <- unlist(strsplit(nombresorig[11], split='[', fixed=TRUE))[2] %>%
substring(., 1, nchar(.)-1)


#Seleccionamos solo casos femeninos y masculinos#
Encuesta <- Encuesta[Encuesta$Sexo %in% c("Masculino","Femenino"),]

#Abrir base con factores de expansion#
FACTOR_EXPANSION_1 <- read_excel("DATOS/FACTOR EXPANSION 1.xlsx")

#Rellenar correos adicionales faltantes#
FACTOR_EXPANSION_1 %<>% mutate(`correo adicional`= ifelse(is.na(`correo adicional`),`Correo Electrónico`,`correo adicional`))

#Contamos casos repetidos en la base fact de exp#
FACTOR_EXPANSION_1 %<>% group_by_all() %>% count

#Homologar nombres de variables con correos electronicos#
names(Encuesta)[43] <- "correo"
names(FACTOR_EXPANSION_1)[5]<- "correo"

# pegado de factor a la base y mantiene todo
#Encuesta <-merge(Encuesta,FACTOR_EXPANSION_1,by = "correo", all.y = TRUE)


#Pega la variable factor en la base encuesta segun correos#
Encuesta %<>% left_join( FACTOR_EXPANSION_1[,c(5:6)], by=c("correo"))

#generar lista de personas que si respondieron Respuesta X y missing en SEXO#
#lista <- as.data.frame(Encuesta$`Correo Electrónico`[is.na(Encuesta$Sexo.y)&Encuesta$RESPUESTAS=="x"])
#listax <- paste0("DATOS/", "lista.xlsx")
#write_xlsx(lista,listax)

#De acuerdo a la variable n se repiten filas#
Encuesta <- data.frame(lapply(Encuesta, rep, Encuesta$n))

#Variable niveles educacion
Encuesta %<>% mutate(edu = recode(Educación, 
"Técnico" = "Otros",
"Universitario" = "Universitario",
"Postgrado (magister o doctorado)" = "Postgrado",
"Licenciado" = "Universitario",
"Universitaria con Postitulos" = "Postgrado",
"Especialización"= "Otros",
.default = ""))


# Colores para gráficos
color <- c( "#CCCCC3", "#88888E", "#DDEE00", "orange","#AA113A")# Create vector of colors
names(color) <- levels(factor(Encuesta$P1_1)) # Extract all levels of both dat

# Tabulados y gráficos de preguntas 1 a 9
P11 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_1)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100)

# Gráfico pregunta 1
g11 <- ggplot(data=P11, aes(y=Freq, x=Var1, fill=Var2)) +
geom_bar(stat="identity",width = 0.8,
 position=position_dodge(width = 1))+
geom_text(aes(label=Freq), vjust=1.6, color="white",
position = position_dodge(0.9), size=3.5)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t11) +
xlab("Sexo") + ylab("Número de casos\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 30), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,30,5)) 
g11

# En porcentajes
g11p <- ggplot(data=P11, aes(y=pct, x=Var1, fill=Var2)) +
geom_bar(stat="identity", position="stack",)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t11)+
xlab("Sexo") + ylab("Porcentaje\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 100), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,100,10)) 
g11p

##############
P12 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_2)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100)

# Gráfico de Pregunta 2
g12 <- ggplot(data=P12, aes(y=Freq, x=Var1, fill=Var2)) +
geom_bar(stat="identity",width = 0.8,
 position=position_dodge(width = 1))+
geom_text(aes(label=Freq), vjust=1.6, color="white",
position = position_dodge(0.9), size=3.5)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t12) +
xlab("Sexo") + ylab("Número de casos\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 30), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,30,5)) 
g12

# En porcentajes
g12p <- ggplot(data=P12, aes(y=pct, x=Var1, fill=Var2)) +
geom_bar(stat="identity", position="stack",)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t12)+
xlab("Sexo") + ylab("Porcentaje\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 100), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,100,10)) 
g12p

##############
P13 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_3)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100)

# Gráfico de la pregunta 3
g13 <- ggplot(data=P13, aes(y=Freq, x=Var1, fill=Var2)) +
geom_bar(stat="identity",width = 0.8,
 position=position_dodge(width = 1))+
geom_text(aes(label=Freq), vjust=1.6, color="white",
position = position_dodge(0.9), size=3.5)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t13) +
xlab("Sexo") + ylab("Número de casos\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 30), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,30,5)) 
g13

# En porcentajes
g13p <- ggplot(data=P13, aes(y=pct, x=Var1, fill=Var2)) +
geom_bar(stat="identity", position="stack",)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t13)+
xlab("Sexo") + ylab("Porcentaje\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 100), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,100,10)) 
g13p


##########
P14 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_4)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100)

# Gráfico de la pregunta 4
g14 <- ggplot(data=P14, aes(y=Freq, x=Var1, fill=Var2)) +
geom_bar(stat="identity",width = 0.8,
 position=position_dodge(width = 1))+
geom_text(aes(label=Freq), vjust=1.6, color="white",
position = position_dodge(0.9), size=3.5)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t14) +
xlab("Sexo") + ylab("Número de casos\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 30), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,30,5)) 
g14

# En porcentajes
g14p <- ggplot(data=P14, aes(y=pct, x=Var1, fill=Var2)) +
geom_bar(stat="identity", position="stack",)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t14)+
xlab("Sexo") + ylab("Porcentaje\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 100), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,100,10)) 
g14p

##########
P15 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_5)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100)

# Gráfico de la pregunta 5
g15 <- ggplot(data=P15, aes(y=Freq, x=Var1, fill=Var2)) +
geom_bar(stat="identity",width = 0.8,
 position=position_dodge(width = 1))+
geom_text(aes(label=Freq), vjust=1.6, color="white",
position = position_dodge(0.9), size=3.5)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t15) +
xlab("Sexo") + ylab("Número de casos\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 30), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,30,5)) 
g15

# En porcentajes
g15p <- ggplot(data=P15, aes(y=pct, x=Var1, fill=Var2)) +
geom_bar(stat="identity", position="stack",)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t15)+
xlab("Sexo") + ylab("Porcentaje\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 100), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,100,10)) 
g15p


#########
P16 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_6)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100)

# Gráfico de la pregunta 6
g16 <- ggplot(data=P16, aes(y=Freq, x=Var1, fill=Var2)) +
geom_bar(stat="identity",width = 0.8,
 position=position_dodge(width = 1))+
geom_text(aes(label=Freq), vjust=1.6, color="white",
position = position_dodge(0.9), size=3.5)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t16) +
xlab("Sexo") + ylab("Número de casos\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 30), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,30,5)) 
g16

# En porcentajes
g16p <- ggplot(data=P16, aes(y=pct, x=Var1, fill=Var2)) +
geom_bar(stat="identity", position="stack",)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t16)+
xlab("Sexo") + ylab("Porcentaje\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 100), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,100,10)) 
g16p

##############
P17 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_7)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100)

# Gráfico de la pregunta 7
g17 <- ggplot(data=P17, aes(y=Freq, x=Var1, fill=Var2)) +
geom_bar(stat="identity",width = 0.8,
 position=position_dodge(width = 1))+
geom_text(aes(label=Freq), vjust=1.6, color="white",
position = position_dodge(0.9), size=3.5)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t17) +
xlab("Sexo") + ylab("Número de casos\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 30), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,30,5)) 
g17

# En porcentajes
g17p <- ggplot(data=P17, aes(y=pct, x=Var1, fill=Var2)) +
geom_bar(stat="identity", position="stack",)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t17)+
xlab("Sexo") + ylab("Porcentaje\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 100), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,100,10)) 
g17p

###########
P18 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_8)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100)

# Gráfico de la pregunta 8
g18 <- ggplot(data=P18, aes(y=Freq, x=Var1, fill=Var2)) +
geom_bar(stat="identity",width = 0.8,
 position=position_dodge(width = 1))+
geom_text(aes(label=Freq), vjust=1.6, color="white",
position = position_dodge(0.9), size=3.5)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t18) +
xlab("Sexo") + ylab("Número de casos\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 30), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,30,5)) 
g18

# En porcentajes
g18p <- ggplot(data=P18, aes(y=pct, x=Var1, fill=Var2)) +
geom_bar(stat="identity", position="stack",)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t18)+
xlab("Sexo") + ylab("Porcentaje\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 100), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,100,10)) 
g18p


#########
P19 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_9)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100)

# Gráfico de la pregunta 9
g19 <- ggplot(data=P19, aes(y=Freq, x=Var1, fill=Var2)) +
geom_bar(stat="identity",width = 0.8,
 position=position_dodge(width = 1))+
geom_text(aes(label=Freq), vjust=1.6, color="white",
position = position_dodge(0.9), size=3.5)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t19) +
xlab("Sexo") + ylab("Número de casos\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 30), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,30,5)) 
g19

# En porcentajes
g19p <- ggplot(data=P19, aes(y=pct, x=Var1, fill=Var2)) +
geom_bar(stat="identity", position="stack",)+
scale_fill_manual(name = "Importancia: ",values = color) +
theme_minimal() +
ggtitle(t19)+
xlab("Sexo") + ylab("Porcentaje\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .8,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 2, 2, 0), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(ylim = c(0, 100), expand = F, clip = "off") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(0,100,10)) 
g19p


#############

# Frecuencia de importancia 5 para pregunas 1 a 9
Encuesta %<>% mutate(P1_1_5 = ifelse(P1_1==5,1,0),
 P1_2_5 = ifelse(P1_2==5,1,0),
 P1_3_5 = ifelse(P1_3==5,1,0),
 P1_4_5 = ifelse(P1_4==5,1,0),
 P1_5_5 = ifelse(P1_5==5,1,0),
 P1_6_5 = ifelse(P1_6==5,1,0),
 P1_7_5 = ifelse(P1_7==5,1,0),
 P1_8_5 = ifelse(P1_8==5,1,0),
 P1_9_5 = ifelse(P1_9==5,1,0))


P115 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_1_5)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100,
 Atributo=t11)

P125 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_2_5)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100,
 Atributo=t12)

P135 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_3_5)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100,
 Atributo=t13)

P145 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_4_5)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100,
 Atributo=t14)

P155 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_5_5)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100,
 Atributo=t15)

P165 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_6_5)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100,
 Atributo=t16)

P175 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_7_5)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100,
 Atributo=t17)

P185 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_8_5)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100,
 Atributo=t18)

P195 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_9_5)) %>%
group_by(Var1) %>%
mutate(pct= Freq/sum(Freq) * 100,
 Atributo=t19)

# Tabla final de porcentajes de importancia 5 en preguntas 1 a 9
P1x5 <- bind_rows(P115, P125, P135, P145, P155, P165, P175, P185, P195)

# Se ordenan según porcentaje 
P1x5 <- P1x5[P1x5$Var2==1,]
sel_order <- 
P1x5 %>% 
filter(Var1 == "Femenino") %>% 
arrange((pct)) %>% 
mutate(labels = factor(Atributo))

# Eiqueta con porcentaje
P1x5 %<>% 
mutate(labels = factor(Atributo, levels = sel_order$labels, ordered = TRUE),
 nlabel = paste0(sprintf("%.1f", pct)))


gp15 <- ggplot(P1x5, aes(x = pct, y = reorder(Atributo, pct))) +
geom_line() +
geom_point(aes(color = Var1), size = 1.5) +
scale_color_brewer(name = "Sexo: ",palette = "Set1", direction = -1) +
theme(legend.position = "bottom") +
geom_text(
aes(label = nlabel, colour = Var1),
size = 2.5,
hjust = 0, position = position_dodge(0.8)
) +
theme_minimal() +
ggtitle("Porcentaje que considera el atributo 
        muy importante por sexo")+
xlab("Porcentaje\n") + ylab("Atributo\n") +
theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
theme(aspect.ratio = .75,
plot.title=element_text(size=14,face="bold"),
plot.subtitle=element_text(size=14),
axis.title=element_text(size=10, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 0.5, 0.5, 1), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
panel.grid.major.x = element_blank()) +
coord_cartesian(xlim = c(0, 100), expand = F, clip = "off") +
scale_x_continuous(breaks=seq(0,100,20)) + 
  theme(axis.title = element_text(size = 8),
  axis.text = element_text(size = 7),
  plot.title = element_text(hjust = .9))


########################
# Frecuencia de importancia 1 o 2 en preguntas 1 a 9
Encuesta %<>% mutate(P1_1_12 = ifelse(P1_1 %in% c(1:2),1,0),
                     P1_2_12 = ifelse(P1_2 %in% c(1:2),1,0),
                     P1_3_12 = ifelse(P1_3 %in% c(1:2),1,0),
                     P1_4_12 = ifelse(P1_4 %in% c(1:2),1,0),
                     P1_5_12 = ifelse(P1_5 %in% c(1:2),1,0),
                     P1_6_12 = ifelse(P1_6 %in% c(1:2),1,0),
                     P1_7_12 = ifelse(P1_7 %in% c(1:2),1,0),
                     P1_8_12 = ifelse(P1_8 %in% c(1:2),1,0),
                     P1_9_12 = ifelse(P1_9 %in% c(1:2),1,0))

P1112 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_1_12)) %>%
  group_by(Var1) %>%
  mutate(pct= Freq/sum(Freq) * 100,
         Atributo=t11)

P1212 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_2_12)) %>%
  group_by(Var1) %>%
  mutate(pct= Freq/sum(Freq) * 100,
         Atributo=t12)

P1312 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_3_12)) %>%
  group_by(Var1) %>%
  mutate(pct= Freq/sum(Freq) * 100,
         Atributo=t13)

P1412 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_4_12)) %>%
  group_by(Var1) %>%
  mutate(pct= Freq/sum(Freq) * 100,
         Atributo=t14)

P1512 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_5_12)) %>%
  group_by(Var1) %>%
  mutate(pct= Freq/sum(Freq) * 100,
         Atributo=t15)

P1612 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_6_12)) %>%
  group_by(Var1) %>%
  mutate(pct= Freq/sum(Freq) * 100,
         Atributo=t16)

P1712 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_7_12)) %>%
  group_by(Var1) %>%
  mutate(pct= Freq/sum(Freq) * 100,
         Atributo=t17)

P1812 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_8_12)) %>%
  group_by(Var1) %>%
  mutate(pct= Freq/sum(Freq) * 100,
         Atributo=t18)

P1912 <- as.data.frame(table(Encuesta$Sexo, Encuesta$P1_9_12)) %>%
  group_by(Var1) %>%
  mutate(pct= Freq/sum(Freq) * 100,
         Atributo=t19)

P1x12 <- bind_rows(P1112, P1212, P1312, P1412, P1512, P1612, P1712, P1812, P1912)


# Se ordenan según porcentaje 

P1x12 <- P1x12[P1x12$Var2==1,]
sel_order <- 
  P1x12 %>% 
  filter(Var1 == "Femenino") %>% 
  arrange((pct)) %>% 
  mutate(labels = factor(Atributo))

P1x12 %<>% 
  mutate(labels = factor(Atributo, levels = sel_order$labels, ordered = TRUE),
         nlabel = paste0(sprintf("%.1f", pct)))

gp112 <- ggplot(P1x12, aes(x = pct, y = reorder(Atributo, pct))) +
  geom_line() +
  geom_point(aes(color = Var1), size = 1.5) +
  scale_color_brewer(name = "Sexo: ",palette = "Set1", direction = -1) +
  theme(legend.position = "bottom") +
  geom_text(
    aes(label = nlabel, colour = Var1),
    size = 2.5,
    hjust = 0, position = position_dodge(0.8)
  ) +
  theme_minimal() +
  ggtitle("Porcentaje que considera el atributo 
poco importante o no importante por sexo")+
  xlab("Porcentaje\n") + ylab("Atributo\n") +
  theme(legend.position = "bottom", legend.direction="horizontal",legend.text = element_text(size=10)) +
  theme(aspect.ratio = .75,
        plot.title=element_text(size=14,face="bold"),
        plot.subtitle=element_text(size=14),
        axis.title=element_text(size=10, vjust = -6),
        plot.caption = element_text(vjust = -45, face = "italic"),
        plot.margin = unit(c(1, 0.5, 0.5, 1), "lines"),
        plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
        panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
        panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
        panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
        panel.grid.major.x = element_blank()) +
  coord_cartesian(xlim = c(0, 8), expand = F, clip = "off") +
  scale_x_continuous(breaks=seq(0,8,2)) + 
  theme(axis.title = element_text(size = 8),
        axis.text = element_text(size = 7),
        plot.title = element_text(hjust = .9))


#######################
# Gráfico con frecuencias de alternativas en porcentaje 
# Preguntas 1 a 9
P11 %<>% mutate(Atributo=t11)
P12 %<>% mutate(Atributo=t12)
P13 %<>% mutate(Atributo=t13)
P14 %<>% mutate(Atributo=t14)
P15 %<>% mutate(Atributo=t15)
P16 %<>% mutate(Atributo=t16)
P17 %<>% mutate(Atributo=t17)
P18 %<>% mutate(Atributo=t18)
P19 %<>% mutate(Atributo=t19)

P1 <- bind_rows(P11, P12, P13, P14, P15, P16, P17, P18, P19)

# En porcentajes
g1p <- ggplot(data=P1, aes(y=pct, x=Var1, fill=Var2)) +
  geom_bar(stat="identity", position="stack", width = .9)+
  scale_fill_manual(name = "Importancia: ",values = color) +
  theme_minimal() +
  facet_wrap(~Atributo,  ncol=2) +
  ggtitle("Distribución de la importancia asignada por atributo y sexo")+
  xlab("Sexo") + ylab("Porcentaje\n") +
  theme(legend.position = c(.75,0.07), legend.direction="vertical", legend.text = element_text(size=10)) +
  theme(aspect.ratio = .2,
        plot.title=element_text(size=12,face="bold"),
        plot.subtitle=element_text(size=10),
        axis.title=element_text(size=8, vjust = -6),
        plot.caption = element_text(vjust = -45, face = "italic"),
        plot.margin = unit(c(1, 1, 1, 0.5), "lines"),
        plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
        panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
        panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
        panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2),
        panel.grid.major.x = element_blank()) +
  coord_cartesian(ylim = c(0, 100), expand = F, clip = "off") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                     breaks=seq(0,100,20)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))





################
## Tabla resumen de porcentajes de preguntas 1 a 9 ###
# Tabla P1 todos los casos
t1 <- proportions(table(Encuesta$P1_1))
t2 <- proportions(table(Encuesta$P1_2))
t3 <- proportions(table(Encuesta$P1_3))
t4 <- proportions(table(Encuesta$P1_4))
t5 <- proportions(table(Encuesta$P1_5))
t6 <- proportions(table(Encuesta$P1_6))
t7 <- proportions(table(Encuesta$P1_7))
t8 <- proportions(table(Encuesta$P1_8))
t9 <- proportions(table(Encuesta$P1_9))
tt <- bind_rows(t1,t2,t3,t4,t5,t6,t7,t8,t9)
tt %<>% mutate(Atributo=c(t11,t12,t13,t14,t15,t16,t17,t18,t19),
 Grupo="Total",
 Total=rowSums(., na.rm=TRUE))

# Tabla P1 Masculino
m1 <- proportions(table(Encuesta$P1_1[Encuesta$Sexo=="Masculino"]))
m2 <- proportions(table(Encuesta$P1_2[Encuesta$Sexo=="Masculino"]))
m3 <- proportions(table(Encuesta$P1_3[Encuesta$Sexo=="Masculino"]))
m4 <- proportions(table(Encuesta$P1_4[Encuesta$Sexo=="Masculino"]))
m5 <- proportions(table(Encuesta$P1_5[Encuesta$Sexo=="Masculino"]))
m6 <- proportions(table(Encuesta$P1_6[Encuesta$Sexo=="Masculino"]))
m7 <- proportions(table(Encuesta$P1_7[Encuesta$Sexo=="Masculino"]))
m8 <- proportions(table(Encuesta$P1_8[Encuesta$Sexo=="Masculino"]))
m9 <- proportions(table(Encuesta$P1_9[Encuesta$Sexo=="Masculino"]))
mt <- bind_rows(m1,m2,m3,m4,m5,m6,m7,m8,m9)
mt %<>% mutate(Atributo=c(t11,t12,t13,t14,t15,t16,t17,t18,t19),
 Grupo="Masculino",
 Total=rowSums(., na.rm=TRUE))

# Tabla P1 Femenino
f1 <- proportions(table(Encuesta$P1_1[Encuesta$Sexo=="Femenino"]))
f2 <- proportions(table(Encuesta$P1_2[Encuesta$Sexo=="Femenino"]))
f3 <- proportions(table(Encuesta$P1_3[Encuesta$Sexo=="Femenino"]))
f4 <- proportions(table(Encuesta$P1_4[Encuesta$Sexo=="Femenino"]))
f5 <- proportions(table(Encuesta$P1_5[Encuesta$Sexo=="Femenino"]))
f6 <- proportions(table(Encuesta$P1_6[Encuesta$Sexo=="Femenino"]))
f7 <- proportions(table(Encuesta$P1_7[Encuesta$Sexo=="Femenino"]))
f8 <- proportions(table(Encuesta$P1_8[Encuesta$Sexo=="Femenino"]))
f9 <- proportions(table(Encuesta$P1_9[Encuesta$Sexo=="Femenino"]))
ft <- bind_rows(f1,f2,f3,f4,f5,f6,f7,f8,f9)
ft %<>% mutate(Atributo=c(t11,t12,t13,t14,t15,t16,t17,t18,t19),
 Grupo="Femenino",
 Total=rowSums(., na.rm=TRUE))

# Tabla completa
TABLAP1 <- bind_rows(tt,mt,ft)

# Reordena columnas
TABLAP1 <- TABLAP1[,c(6,7,1:5,8)]

# Multiplica por 100 para dejar en porcentaje
TABLAP1 %<>%
mutate(across(c(3:8), ~ .*100))

# Ordena por sexo
TABLAP1%<>%
group_by(Atributo) %>%
arrange(desc(Grupo), .by_group=TRUE)

# Deja el nombre del atributo solo una vez
TABLAP1$Atributo[duplicated(TABLAP1$Atributo)] <- ""

# Pasa a formato flextable
TP1 <- flextable(TABLAP1)

TP1 <- colformat_double(
TP1,
big.mark = ",", digits = 1, na_str = "0.0"
)
TP1 <- autofit(TP1)

TP1 <- add_header_lines(TP1, values = "Porcentaje") # add subtitle
TP1 <- add_header_lines(TP1, values = "Distribución de atributos por importancia asignada según sexo") # add title
TP1 <- fontsize(TP1, i = 1, size = 14, part = "header") #increase text size of the title 

TP1 <- set_table_properties(TP1, layout = "autofit")



# Pasa tablas y gráficos a documento word
fileout <- paste0("RESULTADOS/","atributos.docx")

read_docx() %>% 
  body_add_par(value = "Atributos", style = "heading 1") %>% 
  body_add_flextable(value = TP1) %>% 
  body_add_par(value = "", style = "heading 1") %>% 
  body_add_gg(value = g1p, style = "centered", width = 7 ) %>% 
  body_add_gg(value = gp15, style = "centered", width = 6 ) %>% 
  body_add_gg(value = gp112, style = "centered", width = 6 ) %>% 
    print(target = fileout)





