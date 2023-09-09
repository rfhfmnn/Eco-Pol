rm(list=ls())

getwd()
directorio <- "/Users/stefa/Documents/Code/Political_Economy/"
setwd(directorio)

### Instalar y cargar paquetes
install.packages('devtools')
install.packages('wnominate')
install.packages('reshape2')
library(ggplot2)
library(devtools)
devtools::install_github("voteview/Rvoteview", force = TRUE)
library(Rvoteview)
library(pscl)
library(wnominate)
library(reshape2)

##################################
### Interactuando con Voteview ###
##################################

### Consultar base completa y buscar palabras claves
res1 <- voteview_search("Iraq"); names(res1)
res2  <-  voteview_search("Budget")
res3  <-  voteview_search("guns")
res4  <-  voteview_search("abortion")
res5  <- voteview_search("women rights")
res6  <- voteview_search("campaign finance")
res7  <- voteview_search("terrorism iraq")
res8 <- voteview_search("alltext:terrorism iraq")
res9  <-  voteview_search("vote_desc:'iraq'")

### Puede buscarse llamadas de votos de un Congreso espec?fico, dentro de un rango de fechas, y con un cierto nivel de apoyo, y muchas m?s busqueda
res10  <-  voteview_search("tax")
res11 <- voteview_search("tax", startdate = "2005-01-01")
res11b <- voteview_search("tax", startdate = "2005-01-01", chamber = "House")
res11c <- voteview_search("tax", startdate = "2005-01-01", chamber = "Senate")
res12 <- voteview_search("tax", enddate = "2005-01-01", chamber = "House")
res13 <- voteview_search("tax", enddate = "2005-01-01", chamber = "Senate")
res14 <- voteview_search("tax", startdate = "2000-12-20",congress = c(110, 112),chamber = "House")

### B?squedas m?s complejas son posibles
qString <- "alltext:war iraq (enddate:1993 OR startdate:2000)"
res15 <- voteview_search(q = qString)
qString <- "alltext:tax iraq (congress:[100 to 102] AND support:[45 to 55])"
res16 <- voteview_search(q = qString)
qString <- "alltext:tax iraq (congress:[100 to 102] AND vote_result:[Passed])"
res17 <- voteview_search(q = qString)

### Descargar base completa y/o bases acotadas con informaci?n completa de lllamadas de votos
res18 <- voteview_search("'estate tax' congress:105")
rc <- voteview_download(res18$id)
class(rc)
summary(rc)
summary(rc,verbose=TRUE)

### Se puede buscar y descargar por MIEMBRO
clintons <- member_search("clinton")
clintons <- member_search("clinton",state = "NY",distinct = 1)
sanders <- member_search("sanders",state = "VT")
sanders[,names(sanders)!="bio"]

###############################################
###  Estimando y Graficando Puntos Ideales  ###
###############################################

### APLICACI?N: Estimar los puntos ideales de todos los legisladores votando en pol?tica exterior en los primeros 6 meses de la administraci?n de Obama. Usaremos las llamadas de votos que entran el Categor?a Clausen "Foreign and Defense Policy" y que fueran m?s o menos competitivas --entre 15 y 85

res <- voteview_search("codes.Clausen:Foreign and Defense Policy support:[15 to 85]",startdate = "2009-01-20", enddate = "2009-07-20")
rc <- voteview_download(res$id)

### Buscar los "legisladores extremos" para la variable polaridad
cons1 <- rc$legis.long.dynamic[which.max(rc$legis.data$dim1), c("name", "icpsr")]
cons2 <- rc$legis.long.dynamic[which.max(rc$legis.data$dim2), c("name", "icpsr")]
defIdeal <- wnominate(rc,
                      polarity = list("icpsr", c(20753, 20523)))

### Crear las etiquetas de nombres de partidos y graficar
defIdeal$legislators$partyName <- ifelse(defIdeal$legislators$party == 200, "Republican",ifelse(defIdeal$legislators$party == 100, "Democrat", "Independent"))

ggplot(defIdeal$legislators,aes(x=coord1D, y=coord2D, color=partyName, label=state_abbrev)) +  geom_text() + scale_color_manual("Party", values = c("Republican" = "red","Democrat" = "blue","Independent" = "darkgreen"))+labs(title="Ideal Point Estimation - Legislators voting on foreign policy during the first six months of Obama's presidency") + theme_bw()


####************** HASTA AC? ESTA VERIFICADO ******************########


##############################################
### Regression Analysis of Roll Call Votes ###
##############################################

### APLICACI?N: Usar encuestas de opini?n p?blica sobre derechos de comunidad LGBT (Lax and Phillips, 2009). Usan regresi?n multinivel con encuestas con post-estratificaci?n. Pueden usarse algunas leyes que pasaron por el Congreos en esos a?os

res2 <- voteview_search("codes.Issue:Homosexuality congress:111")
res[1:5,1:10]

### Focus on Don't Ask, Don't Tell and Hate Crimes
dadt <- voteview_download(c("RH1111621", "RS1110678", "RH1110222"))
dadt$vote.data

dadtLong <- melt_rollcall(dadt,legiscols = c("name", "state_abbrev","party_code", "dim1", "dim2"),votecols = c("vname", "date", "chamber"))
head(dadtLong)

data(states)
dadtLong <- merge(dadtLong, states[, c("state_abbrev", "state_name")],by = "state_abbrev")
dadtLong$state_name <- tolower(dadtLong$state_name)

### Merge with Lax & Phillips (2009)
data(lpOpinion)
lpOpinion$state <- tolower(lpOpinion$state)
df <- merge(dadtLong, lpOpinion,by.x = "state_name.x", by.y = "state")
head(df)

## Recode votes
df$voteYes <- ifelse(df$vote == 1, 1, ifelse(df$vote == 6, 0, NA))

## Raw votes by party
table(df$party_code, df$voteYes, useNA = "always")

## Recode party (add independent to democrats)
df$republican <- ifelse(df$party_code == "200", 1, 0)

## Simple model
summary(lm(voteYes ~ meanOpinion, data = df))

## Control for party
summary(lm(voteYes ~ meanOpinion*republican, data = df))

## Control for ideology
## Note that ideology here has been estimated using these and later votes,
## so interpret the results with some caution
summary(lm(voteYes ~ meanOpinion*republican + dim1 + dim2,data = df))

##### REVISAR Y EDITAR ######

### Y tambi?n otros m?todos gr?ficos!
plot(defIdeal,colour=partyName)

### Tambi?n podemos usar el objeto "rollcall" en el paquete pscl
defIdeal <- ideal(rc,d = 2)
plot(defIdeal)

### Usando res$id podemos obtener un objeto de llamada de votos (del paquete 'pcsl')que contiene el conjunto total de votes y datos para cada llamada de votos.
rc <- voteview_download(res$id[1:10])
summary(rc)

### Se puede acceder a datos de legisladores puntuales
rc$legis.long.dynamic[1:5, 1:5]

### PARTY POLARIZATION
nom_dat <- read.csv("https://voteview.com/static/data/out/members/HSall_members.csv")
pol  <-  read.csv("voteview_polarization_data.csv")
