getwd()
install.packages('openxlsx')
install.packages('dplyr')
install.packages("ggplot")

library(openxlsx)
library(dplyr)

df<-read.xlsx('preferencias_candidatos.xlsx')

# ------ selección y ordenamiento de columnas ------

df_red<-df[,2:6]
df_red<-df_red[,c(5,1,3,2,4)]

# ------ reemplazo valores y traspongo el df para poder graficar mejor ------

df_red1<-replace(df_red,df_red==1,5)
df_red2<-replace(df_red1,df_red==2,4)
df_red3<-replace(df_red2,df_red==3,3)
df_red4<-replace(df_red3,df_red==4,2)
df_red5<-replace(df_red4,df_red==5,1)
df_transpose <- data.frame(t(df_red5[]))

# ------ separo las columnas y creo una lista para cada una ------

listas<-lapply(df_transpose,as.list)

# ------ creo df nuevo para hacer gráfico de lineas con datos categóricos ------

datos<-data.frame(candidatos=as.factor(c('1.Grabois','2.Massa','3.Larreta','4.Bullrich','5.Milei')),
                  individuos=df_transpose)
head(datos)

# ------ gráfico ------

par(mfrow=c(3,4))
for(mi_lista in listas){
  plot(as.numeric(datos$candidatos),
       mi_lista,
       type = "b",col=3,lwd=2,
       ylab = "Valoración", 
       xlab = "Candidatos",
       xaxt = "n")
  axis(1, labels = as.character(datos$candidatos),
       at = as.numeric(datos$candidatos))
}