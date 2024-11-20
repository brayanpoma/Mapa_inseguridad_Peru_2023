#### MAPA DE INSEGURIDAD PERÚ - 2023 ####

#Configuraciones iniciales.
rm(list=ls())
dev.off()
setwd("D:/PROYECTOS/Delincuencia/Mapa_inseguridad")
getwd()
list.files()

#Packages
library(tidyverse)      #Data manipulation
library(naniar)         #Missing values
library(rnaturalearth)  #Maps
library(gridExtra)      #Join graphs
library(grid)           #Join graphs
library(haven)          #SPSS files      

#### Extracción de datos. ####
link<-"https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS/903-Modulo1819.zip"
año<-"2023"

descarga<-function(link,año){
  nombrezip<-gsub(".+SPSS/","",link)
  nombrecarpeta<-gsub("[.]zip","",nombrezip)
  download.file(link,nombrezip)
  unzip(nombrezip,exdir = ".")
  #Renombrar la carpeta con el año al que pertenece
  if (file.exists(nombrecarpeta)) {
    file.rename(nombrecarpeta, año)
  } else {
    message("La carpeta no existe")
  }
}

descarga(link,año)
df_2023<-read_sav(paste0(getwd(),"/",año,"/","CAP_600_URBANO_7.sav"))

#### Limpieza de datos ####

df_2023[]<-lapply(df_2023, FUN = function(x){
  attributes(x)<-NULL
  x
})

#Seleccionamos los hechos delictivos que atentan contra la seguridad de la población

#P601_1:  ROBO DE VEHICULO AUTOMOTOR - AUTO,CAMIONETA: 1:Sí, 2:No, 3:No tiene
#P601_3A: ROBO DE AUTOPARTES: 1:Sí, 2:No, 3:No tiene
#P601_4A: ROBO DE MOTO-MOTOTAXI: 1:Sí, 2:No, 3:No tiene
#P601_5A: ROBO DE BICICLETA: 1:Sí, 2:No, 3:No tiene
#P601_6A: ROBO DE DINERO CARTERA CELULAR ETC.: 1:Sí, 2:No, 3:No tiene
#P601_7:  AMENAZAS E INTIMIDACIONES: 1:Sí, 2:No, 3:No tiene
#P601_8:  MALTRATO FISICO Y/O PSICOLOGICO DE ALGUN MIEMBRO DE SU HOGAR: 1:Sí, 2:No, 3:No tiene
#P601_9:  OFENSAS SEXUALES (ACOSO,ABUSO,VIOLACION,ETC):
#P601_10: VICTIMA DE SECUESTRO: 1:Sí, 2:No, 3:No tiene
#P601_12: EXTORSION: 1:Sí, 2:No, 3:No tiene
#P601_13: ESTAFA: 1:Sí, 2:No, 3:No tiene
#P601_13_ESPECIF: ESTAFA_ESPECIFIQUE: 
#P601_14: ROBO DE NEGOCIO: 1:Sí, 2:No, 3:No tiene
#P601_15: ¿UD. HA SIDO VICTIMA DE OTRO: 1:Sí, 2:No, 3:No tiene
#P601_15_O: OTRO_ESPECIFIQUE
#P601_16: DELITOS INFORMATICOS: 1:Sí, 2:No, 3:No tiene
#P601_16_ESPECIF: DELITOS INFORMATICOS ESPECIFIQUE:
#P601_2:  INTENTO DE DE ROBO DE VEHICULO AUTOMOTOR (AUTO, CAMIONETA, ETC.): 1:Sí, 2:No, 3:No tiene
#P601_3B: INTENTO DE ROBO DE AUTOPARTES DEL VEHICULO AUTOMOTOR: 1:Sí, 2:No, 3:No tiene
#P601_4B: INTENTO DE ROBO DE MOTOCICLETA/MOTOTAXI. 1:Sí, 2:No, 3:No tiene
#P601_5B: INTENTO DE ROBO DE BICICLETA: 1:Sí, 2:No, 3:No tiene
#P601_6B: INTENTO DE ROBO DE DINERO, CARTERA, CELULAR, ETC: 1:Sí, 2:No, 3:No tiene
#P601_11: INTENTO DE SECUESTRO: 1:Sí, 2:No, 3:No tiene
#P601_12A: INTENTO DE EXTORSION: 1:Sí, 2:No, 3:No tiene

variables<-df_2023 |> 
  select(starts_with("P601")) |> 
  names()
  
df_2023<-df_2023 |> 
  select(starts_with("P601"),NOMBREDD)
glimpse(df_2023)
df_2023[df_2023==""]<-NA

miss_var_summary(df_2023)
miss_var_table(df_2023)

filtrar_var<-function(df){
  var_prop_na<-(colSums(is.na(df))/nrow(df))*100
  df<-df |> 
    select(-names(var_prop_na[var_prop_na>10]))
}

df_2023<-filtrar_var(df_2023)

miss_var_summary(df_2023)
miss_var_table(df_2023)
gg_miss_var(df_2023)
gg_miss_case(df_2023)

df_2023<-na.omit(df_2023)
glimpse(df_2023)

#### Transformación de datos ####

inseguridad_dptos<-df_2023 |> 
  mutate(victima=as.integer(rowSums(df_2023[variables]==1)>0)) |> 
  group_by(NOMBREDD) |> 
  summarise(N=n(),
            vic=sum(victima)) |> 
  mutate(prop=(vic/N)*100)


departamentos <- ne_states(country = "Peru", returnclass = "sf")
departamentos$name
departamentos<-departamentos |> 
  filter(name!="Lima Province")

nombres_dptos<-sort(departamentos$name)
inseguridad_dptos$NOMBREDD<-nombres_dptos
mapa_delincuencia <- departamentos |> 
  left_join(inseguridad_dptos, by = c("name" = "NOMBREDD"))



#### VISUALIZACION ####

barra<-inseguridad_dptos |> 
  ggplot(aes(x=fct_reorder(NOMBREDD,prop),y=prop))+
  geom_bar(stat = "identity",aes(fill = prop))+
  coord_flip()+
  theme_classic()+
  scale_fill_gradient(low = "#f3cec3", high = "#d30324",guide = "none")+
  geom_text(aes(label = sprintf("%.1f", prop)),
            hjust=1.5,
            size=3.5)+
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black"))

mapa<-ggplot(mapa_delincuencia) +
  geom_sf(aes(fill = prop), color = "white") +
  scale_fill_gradient(low = "#f3cec3", high = "#d30324", guide = "none") +
  theme_classic()+
  geom_sf_text(aes(label =name),
               size=2.8)+
  theme(axis.line = element_blank(),
        axis.ticks =element_blank(),
        axis.text = element_blank())+
  ylab("")+
  xlab("")


# Uniendo los graficos.

Mapa3<-grid.arrange(mapa, barra, ncol = 2, widths = c(2, 1))

Mapa3 <- grid.arrange(
  top = arrangeGrob( #inseguridad
    textGrob("MAPA DE INSEGURIDAD - PERÚ 2023", gp = gpar(fontsize = 15,fontface = "bold"),
             vjust =1 ),
    textGrob("(Víctimas de hechos delictivos - Porcentaje)",gp = gpar(fontsize = 10, fontface = "italic"),
             vjust = 2.5),
    ncol = 1 ),
  bottom = textGrob("Fuente: INEI - Encuesta Nacional de Programas Presupuestales 2023.", 
                    gp = gpar(fontsize = 10)),
  mapa, barra, ncol = 2, widths = c(2, 1)
)



