# Economía Política 2023 - FCE UNC - Córdoba, Argentina #

rm(list=ls())

getwd()
directorio <- "/Users/stefa/Documents/Code/Eco-Pol/Voteview/"
setwd(directorio)

##############################################################################################################

# Paquetes #
install.packages('wnominate','devtools')
library(ggplot2)
library(devtools)
devtools::install_github("voteview/Rvoteview")
library(Rvoteview)
library(pscl)
library(wnominate)

##############################################################################################################
########## IMPACTOS GUERRAS SOBRE VOTOS LEGISLATIVOS #########################################################
##############################################################################################################

## SEGUNDA GUERRA MUNDIAL ####################################################################################

# Puntos ideales #
{
print("tranqui tarda un poco, prepara el mate")
res_pre2GM <- voteview_search("codes.Clausen:Foreign and Defense Policy support:[15 to 85]",startdate = "1946-01-20", enddate = "1950-01-20")
res_post2GM <- voteview_search("codes.Clausen:Foreign and Defense Policy support:[15 to 85]",startdate = "1935-01-20", enddate = "1939-01-20")
res_dur2GM <- voteview_search("codes.Clausen:Foreign and Defense Policy support:[15 to 85]",startdate = "1940-01-20", enddate = "1945-01-20")

rc_pre2GM <- voteview_download(res_pre2GM$id)
rc_post2GM <- voteview_download(res_post2GM$id)
rc_dur2GM <- voteview_download(res_dur2GM$id)
}

# Buscar los "legisladores extremos" para la variable Polaridad #
{
cons1_pre2GM <- rc_pre2GM$legis.long.dynamic[which.max(rc_pre2GM$legis.data$dim1), c("name", "icpsr")]
cons2_pre2GM <- rc_pre2GM$legis.long.dynamic[which.max(rc_pre2GM$legis.data$dim2), c("name", "icpsr")]
defIdeal_pre2GM <- wnominate(rc_pre2GM,
                      polarity = list("icpsr", c(cons1_pre2GM$icpsr, cons2_pre2GM$icpsr)))
cons1_dur2GM <- rc_dur2GM$legis.long.dynamic[which.max(rc_dur2GM$legis.data$dim1), c("name", "icpsr")]
cons2_dur2GM <- rc_dur2GM$legis.long.dynamic[which.max(rc_dur2GM$legis.data$dim2), c("name", "icpsr")]
defIdeal_dur2GM <- wnominate(rc_dur2GM,
                      polarity = list("icpsr", c(cons1_dur2GM$icpsr, cons2_dur2GM$icpsr)))
cons1_post2GM <- rc_post2GM$legis.long.dynamic[which.max(rc_post2GM$legis.data$dim1), c("name", "icpsr")]
cons2_post2GM <- rc_post2GM$legis.long.dynamic[which.max(rc_post2GM$legis.data$dim2), c("name", "icpsr")]
defIdeal_post2GM <- wnominate(rc_post2GM,
                      polarity = list("icpsr", c(cons1_post2GM$icpsr, cons2_post2GM$icpsr)))

}
cons1_pre2GM
cons2_pre2GM
cons1_dur2GM
cons2_dur2GM
cons1_post2GM
cons2_post2GM

# Crear las etiquetas de nombres de partidos y graficar #
dir.create("Graphs")

{
defIdeal_pre2GM$legislators$partyName <- ifelse(defIdeal_pre2GM$legislators$party == 200, "Republican",ifelse(defIdeal_pre2GM$legislators$party == 100, "Democrat", "Independent"))
graph_pre2GM <- ggplot(defIdeal_pre2GM$legislators,aes(x=coord1D, y=coord2D, color=partyName, label=state_abbrev)) +  geom_text() + scale_color_manual("Party", values = c("Republican" = "red","Democrat" = "blue","Independent" = "darkgreen")) + theme_bw() + labs(x = "Economics", y = "Social", title = "Previo a la 2GM")
ggsave(filename = "Graphs/pre_2GM.jpg", plot = graph_pre2GM, width = 6, height = 4)

defIdeal_dur2GM$legislators$partyName <- ifelse(defIdeal_dur2GM$legislators$party == 200, "Republican",ifelse(defIdeal_dur2GM$legislators$party == 100, "Democrat", "Independent"))
graph_dur2GM <- ggplot(defIdeal_dur2GM$legislators,aes(x=coord1D, y=coord2D, color=partyName, label=state_abbrev)) +  geom_text() + scale_color_manual("Party", values = c("Republican" = "red","Democrat" = "blue","Independent" = "darkgreen")) + theme_bw() + labs(x = "Economics", y = "Social", title = "Durante la 2GM")
ggsave(filename = "Graphs/dur_2GM.jpg", plot = graph_dur2GM, width = 6, height = 4)

defIdeal_post2GM$legislators$partyName <- ifelse(defIdeal_post2GM$legislators$party == 200, "Republican",ifelse(defIdeal_post2GM$legislators$party == 100, "Democrat", "Independent"))
graph_post2GM <- ggplot(defIdeal_post2GM$legislators,aes(x=coord1D, y=coord2D, color=partyName, label=state_abbrev)) +  geom_text() + scale_color_manual("Party", values = c("Republican" = "red","Democrat" = "blue","Independent" = "darkgreen")) + theme_bw() + labs(x = "Economics", y = "Social", title = "Luego de la 2GM")
ggsave(filename = "Graphs/post_2GM.jpg", plot = graph_post2GM, width = 6, height = 4)
}


## GUERRA DE VIETNAM #######################################################################################

# Puntos ideales #
{
print("tranqui tarda un poco, prepara el mate")
res_preV <- voteview_search("codes.Clausen:Foreign and Defense Policy support:[15 to 85]",startdate = "1955-01-20", enddate = "1964-01-20")
res_postV <- voteview_search("codes.Clausen:Foreign and Defense Policy support:[15 to 85]",startdate = "1965-01-20", enddate = "1975-01-20")
res_durV <- voteview_search("codes.Clausen:Foreign and Defense Policy support:[15 to 85]",startdate = "1976-01-20", enddate = "1985-01-20")
  
rc_preV <- voteview_download(res_preV$id)
rc_postV <- voteview_download(res_postV$id)
rc_durV <- voteview_download(res_durV$id)
}

# Buscar los "legisladores extremos" para la variable Polaridad #
{
  cons1_preV <- rc_preV$legis.long.dynamic[which.max(rc_preV$legis.data$dim1), c("name", "icpsr")]
  cons2_preV <- rc_preV$legis.long.dynamic[which.max(rc_preV$legis.data$dim2), c("name", "icpsr")]
  defIdeal_preV <- wnominate(rc_preV,
                               polarity = list("icpsr", c(cons1_preV$icpsr, cons2_preV$icpsr)))
  cons1_durV <- rc_durV$legis.long.dynamic[which.max(rc_durV$legis.data$dim1), c("name", "icpsr")]
  cons2_durV <- rc_durV$legis.long.dynamic[which.max(rc_durV$legis.data$dim2), c("name", "icpsr")]
  defIdeal_durV <- wnominate(rc_durV,
                               polarity = list("icpsr", c(cons1_durV$icpsr, cons2_durV$icpsr)))
  cons1_postV <- rc_postV$legis.long.dynamic[which.max(rc_postV$legis.data$dim1), c("name", "icpsr")]
  cons2_postV <- rc_postV$legis.long.dynamic[which.max(rc_postV$legis.data$dim2), c("name", "icpsr")]
  defIdeal_postV <- wnominate(rc_postV,
                                polarity = list("icpsr", c(cons1_postV$icpsr, cons2_postV$icpsr)))
  
}
cons1_preV
cons2_preV
cons1_durV
cons2_durV
cons1_postV
cons2_postV

# Crear las etiquetas de nombres de partidos y graficar #
dir.create("Graphs")

{
  defIdeal_preV$legislators$partyName <- ifelse(defIdeal_preV$legislators$party == 200, "Republican",ifelse(defIdeal_preV$legislators$party == 100, "Democrat", "Independent"))
  graph_preV <- ggplot(defIdeal_preV$legislators,aes(x=coord1D, y=coord2D, color=partyName, label=state_abbrev)) +  geom_text() + scale_color_manual("Party", values = c("Republican" = "red","Democrat" = "blue","Independent" = "darkgreen")) + theme_bw() + labs(x = "Economics", y = "Social", title = "Previo a la Guerra de Vietnam")
  ggsave(filename = "Graphs/pre_V.jpg", plot = graph_preV, width = 6, height = 4)
  
  defIdeal_durV$legislators$partyName <- ifelse(defIdeal_durV$legislators$party == 200, "Republican",ifelse(defIdeal_durV$legislators$party == 100, "Democrat", "Independent"))
  graph_durV <- ggplot(defIdeal_durV$legislators,aes(x=coord1D, y=coord2D, color=partyName, label=state_abbrev)) +  geom_text() + scale_color_manual("Party", values = c("Republican" = "red","Democrat" = "blue","Independent" = "darkgreen")) + theme_bw() + labs(x = "Economics", y = "Social", title = "Durante la Guerra de Vietnam")
  ggsave(filename = "Graphs/dur_V.jpg", plot = graph_durV, width = 6, height = 4)
  
  defIdeal_postV$legislators$partyName <- ifelse(defIdeal_postV$legislators$party == 200, "Republican",ifelse(defIdeal_postV$legislators$party == 100, "Democrat", "Independent"))
  graph_postV <- ggplot(defIdeal_postV$legislators,aes(x=coord1D, y=coord2D, color=partyName, label=state_abbrev)) +  geom_text() + scale_color_manual("Party", values = c("Republican" = "red","Democrat" = "blue","Independent" = "darkgreen")) + theme_bw() + labs(x = "Economics", y = "Social", title = "Luego de la Guerra de Vietnam")
  ggsave(filename = "Graphs/post_V.jpg", plot = graph_postV, width = 6, height = 4)
}


## INVASION DE AFGANISTAN (2001) #############################################################################

# Puntos ideales #
{
  print("tranqui tarda un poco, prepara el mate")
  res_preA <- voteview_search("codes.Clausen:Foreign and Defense Policy support:[15 to 85]",startdate = "1995-01-20", enddate = "2000-01-20")
  res_postA <- voteview_search("codes.Clausen:Foreign and Defense Policy support:[15 to 85]",startdate = "2001-01-20", enddate = "2005-01-20")
  rc_preA <- voteview_download(res_preA$id)
  rc_postA <- voteview_download(res_postA$id)
}

# Buscar los "legisladores extremos" para la variable Polaridad #
{
  cons1_preA <- rc_preA$legis.long.dynamic[which.max(rc_preA$legis.data$dim1), c("name", "icpsr")]
  cons2_preA <- rc_preA$legis.long.dynamic[which.max(rc_preA$legis.data$dim2), c("name", "icpsr")]
  defIdeal_preA <- wnominate(rc_preA,
                               polarity = list("icpsr", c(cons1_preA$icpsr, cons2_preA$icpsr)))
  cons1_postA <- rc_postA$legis.long.dynamic[which.max(rc_postA$legis.data$dim1), c("name", "icpsr")]
  cons2_postA <- rc_postA$legis.long.dynamic[which.max(rc_postA$legis.data$dim2), c("name", "icpsr")]
  defIdeal_postA <- wnominate(rc_postA,
                                polarity = list("icpsr", c(cons1_postA$icpsr, cons2_postA$icpsr)))
  
}
cons1_preA
cons2_preA
cons1_postA
cons2_postA

# Crear las etiquetas de nombres de partidos y graficar #
dir.create("Graphs")

{
  defIdeal_preA$legislators$partyName <- ifelse(defIdeal_preA$legislators$party == 200, "Republican",ifelse(defIdeal_preA$legislators$party == 100, "Democrat", "Independent"))
  graph_preA <- ggplot(defIdeal_preA$legislators,aes(x=coord1D, y=coord2D, color=partyName, label=state_abbrev)) +  geom_text() + scale_color_manual("Party", values = c("Republican" = "red","Democrat" = "blue","Independent" = "darkgreen")) + theme_bw() + labs(x = "Economics", y = "Social", title = "Previo a la invasión de Afganistan")
  ggsave(filename = "Graphs/pre_A.jpg", plot = graph_preA, width = 6, height = 4)
  
  defIdeal_postA$legislators$partyName <- ifelse(defIdeal_postA$legislators$party == 200, "Republican",ifelse(defIdeal_postA$legislators$party == 100, "Democrat", "Independent"))
  graph_postA <- ggplot(defIdeal_postA$legislators,aes(x=coord1D, y=coord2D, color=partyName, label=state_abbrev)) +  geom_text() + scale_color_manual("Party", values = c("Republican" = "red","Democrat" = "blue","Independent" = "darkgreen")) + theme_bw() + labs(x = "Economics", y = "Social", title = "Luego de la invasión de Afganistan")
  ggsave(filename = "Graphs/post_A.jpg", plot = graph_postA, width = 6, height = 4)
}



##############################################################################################################
##############################################################################################################

##### Estimación de puntos ideales -- APLICACIÓN: Estimar los puntos ideales de todos los legisladores votando en política exterior en los primeros 6 meses de la administración de Obama. Usaremos las llamadas de votos que entran el Categoría Clausen "Foreign and Defense Policy" y que fueran más o menos competitivas --entre 15 y 85 de los votos YES
#res <- voteview_search("codes.Clausen:Foreign and Defense Policy support:[15 to 85]",startdate = "1975-01-20", enddate = "1979-01-20")
#rc <- voteview_download(res$id)

##### Buscar los "legisladores extremos" para la variable Polaridad
#cons1 <- rc$legis.long.dynamic[which.max(rc$legis.data$dim1), c("name", "icpsr")]; cons1
#cons2 <- rc$legis.long.dynamic[which.max(rcM$legis.data$dim2), c("name", "icpsr")]; cons2
#defIdeal <- wnominate(rc,
#                      polarity = list("icpsr", c(cons1$icpsr, cons2$icpsr)))

##### Crear las etiquetas de nombres de partidos y graficar
#defIdeal$legislators$partyName <- ifelse(defIdeal$legislators$party == 200, "Republican",ifelse(defIdeal$legislators$party == 100, "Democrat", "Independent"))
#graph <- ggplot(defIdeal$legislators,aes(x=coord1D, y=coord2D, color=partyName, label=state_abbrev)) +  geom_text() + scale_color_manual("Party", values = c("Republican" = "red","Democrat" = "blue","Independent" = "darkgreen")) + theme_bw()
#ggsave(filename = "Graphs/4ypost_Vietnam.jpg", plot = graph, width = 6, height = 4)












