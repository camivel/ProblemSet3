#Descriptivas
setwd("C:/Users/Diana Contreras/OneDrive - Universidad de los Andes/1.Big Data/Problem Set 3/Descriptiva")

#install.packages("gtsummary")
require(pacman)
library(cowplot) #Gráficas en una sola
library(gtsummary)
library(officer)
library(flextable)
p_load(class)
p_load(tidyverse,rio,
       sf, # Leer/escribir/manipular datos espaciales
       leaflet, # Visualizaciones dinámicas
       tmaptools, # geocode_OSM()
       osmdata) # Get OSM's data

library(ggplot2)

#Nos quedamos solo con la base train

train_final<-filter(base_completa, base=="train")
train_final<-select(train_final, subset=-c(base))
test_final<-filter(base_completa, base=="test")
test_final<-select(test_final, subset=-c(base))

#Al train le tenemos que pegar la variabl y

precios<-select(train, property_id, price)
train_final<-left_join(precios, train_final, "property_id")

#ANALISIS DE DATOS ATIPICOS- Densidad-------------------------------------------
dist_price<- ggplot(train_final, aes(x=price)) + 
  geom_histogram(aes(y=..density..),colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + # Overlay with transparent density plot
  ggtitle("Distribución precio vivienda") +
  theme_bw()+
  labs(y="Densidad", x="Precio") + 
  theme_classic() +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.title = element_text(size=10,face="bold"))

summary(train_final$price)

train_final$log_price=log(train_final$price)#creo la variable en lo

dist_lprice<- ggplot(train_final, aes(x=log_price)) + 
  geom_histogram(aes(y=..density..),colour="black", fill="white") +
  geom_density(alpha=.2, fill="#9933FF") + # Overlay with transparent density plot
  ggtitle("Distribución log  precio") +
  theme_bw()+
  labs(y="Densidad", x="log(Precio)") + 
  theme_classic()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.title = element_text(size=10,face="bold"))

#Vamos a cortar valores atipicos

train_final_fil<-filter(train_final, price<=3000000000)
train_final_fil<-filter(train_final_fil, price>=100000000)#Quitamos en total # observaciones

dist_pricefi<- ggplot(train_final_fil, aes(x=price)) + 
  geom_histogram(aes(y=..density..),colour="black", fill="white") +
  geom_density(alpha=.2, fill="#33FFFF") + # Overlay with transparent density plot
  ggtitle("Dist.precio sin valores extremos") +
  theme_bw()+
  labs(y="Densidad", x="Precio") + 
  theme_classic()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.title = element_text(size=10,face="bold"))

dist_lpricefi<- ggplot(train_final_fil, aes(x=log_price)) + 
  geom_histogram(aes(y=..density..),colour="black", fill="white") +
  geom_density(alpha=.2, fill="darkgrey") + # Overlay with transparent density plot
  ggtitle("Dist.log precio sin valores extremos") +
  theme_bw()+
  labs(y="Densidad", x="log(Precio)") + 
  theme_classic()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.title = element_text(size=10,face="bold"))
#Todo en una misma gráfica
plot_grid(dist_price,dist_lprice, dist_pricefi,dist_lpricefi, nrow=2,ncol=2)
#Correlograma-------------------------------------------------------------------
#Anlizamos para baños
#Vamos a eliminar las observaciones con más de nueve años

train_final_fil2<-filter(train_final_fil, banos_new<9)
train_final_fil2<-filter(train_final_fil2, bedrooms>0)

boxp_est<-ggplot(data = train_final_fil, aes(x=estrato_new, y=log_price)) +
  geom_boxplot(fill="steelblue") +
  labs(title="Log(precio) por estrato", x="Estrato", y="log(precio)") +
  theme_classic()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.title = element_text(size=10,face="bold"))
boxp_ciudad<-ggplot(data = train_final_fil, aes(x=l3, y=log_price)) +
  geom_boxplot(fill="orange") +
  labs(title="Log(precio) por ciudad", x="Ciudad", y="log(precio)") +
  theme_classic()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.title = element_text(size=10,face="bold"))
boxp_bano<-ggplot(data = train_final_fil2, aes(x=factor(banos_new), y=log_price)) +
  geom_boxplot(fill="pink") +
  labs(title="Log(precio) por No.baños", x="No.baños", y="log(precio)") +
  theme_classic()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.title = element_text(size=10,face="bold"))
boxp_dormi<-ggplot(data = train_final_fil2, aes(x=factor(banos_new), y=log_price)) +
  geom_boxplot(fill="red") +
  labs(title="Log(precio) por No.dormitorios", x="No.Dormitorios", y="log(precio)") +
  theme_classic()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.title = element_text(size=10,face="bold"))
plot_grid(boxp_ciudad, boxp_est, boxp_bano, boxp_dormi, nrow=2,ncol=2)

#Geom point---------------------------------------------------------------------
train_final_fil3<-filter(train_final_fil2, surface_new<1000) #eliminamos observacion atipica

ggplot(train_final_fil2, aes(x=surface_new, y=log_price)) +
  geom_point(shape=23, fill="blue", color="darkred", size=1)

#Tabla--------------------------------------------------------------------
df3<-select(scompleta, price, log_price, l3, property_type, bedrooms, banos_new, 
            estrato_new, surface_new, dist_estacion, distancia_mall)
df3<-st_drop_geometry(df3)
tbl_descriptivas <- df3 %>%
  select(-log_price) %>%
  tbl_summary(
    by= l3,
    statistic= list(
      all_continuous()~"{mean} ({sd})",
      all_categorical()~"{n} ({p}%)"),
    label = list(price~ "Precio vivienda", property_type ~ "Tipo de propiedad",
            bedrooms ~ "Dormitorios", banos_new ~ "Baños",
            estrato_new ~ "Estrato", surface_new~"Superficie (m2)",
            dist_estacion~"Dist. min a una estación Transmilenio/Metro",
            distancia_mall~"Dist. min a un CC"),
    type= banos_new ~ "continuous") %>%
  modify_caption("**Tabla 1. Estadisticas descriptivas por ciudad**") %>% 
  modify_footnote(all_stat_cols()~ "Promedio (Est.Desv)/ Frecuencia(%)")%>%
  as_flex_table() %>%
  flextable::save_as_docx(path="Descriptivas1.docx")
  
tbl_descriptiva2<- df3 %>%
  select(l3, price, bedrooms, banos_new, surface_new, 
         dist_estacion, distancia_mall) %>%
  tbl_summary(
    by= l3,
    statistic= list(all_continuous()~"{mean} ({sd})"),
    label = list(price~ "Precio vivienda",bedrooms ~ "Dormitorios", 
                 banos_new ~ "Baños", surface_new~"Superficie (m2)",
                 dist_estacion~"Dist. min a una estación Transmilenio/Metro",
                 distancia_mall~"Dit. min a un CC"),
    type= banos_new ~ "continuous") %>%
  add_difference()%>%
  modify_caption("**Tabla 2. T-test entre ciudad**") %>% 
  modify_footnote(all_stat_cols()~"Promedio (Est.Desv)") %>%
  as_flex_table() %>%
  flextable::save_as_docx(path="Descriptivas2.docx")

