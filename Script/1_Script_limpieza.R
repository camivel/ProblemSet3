### Problem set 3
setwd("C:/Users/Diana Contreras/OneDrive - Universidad de los Andes/1.Big Data/Problem Set 3")

#install.packages("pacman")
require(pacman)
library(cowplot)
p_load(class)
p_load(tidyverse,rio,
       sf, # Leer/escribir/manipular datos espaciales
       leaflet, # Visualizaciones dinámicas
       tmaptools, # geocode_OSM()
       osmdata) # Get OSM's data
train$base<-"train"
train2<-subset(train, select=-c(price))
test$base<-"test"

df<-rbind(train2, test)
table(is.na(df$surface_total))
table(is.na(df$bathrooms))

#Creamos la variable geometrica

df = st_as_sf(df,coords=c("lon","lat"),crs=4326)

#todo a letra minuscula

df$description<-sapply(df$description, str_to_lower)

#Vamos a crear nuevas variables a partir del texto 
df$description[1]
df$description[111]
df$surface_covered[111]

#Identificar patrones de AREA

a = "[:space:]+[:digit:]+[:space:]+metros"
b = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+metros"
c = "[:space:]+[:digit:]+[:space:]+mts"
d = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mts"
e = "[:space:]+[:digit:]+[:space:]+mt2"
f = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mt2"
g = "[:space:]+[:digit:]+[:space:]+m2"
h = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+m2"
i = "[:space:]+[:digit:]+[:space:]+mts2"
j = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mts2"


df = df %>% 
  mutate(new_surface = str_extract(string=df$description , pattern= a))
table(df$new_surface)

df = df %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=df$description , pattern= b),
                              new_surface))
df = df %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=df$description , pattern= c),
                              new_surface))
df = df %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=df$description , pattern= d),
                              new_surface))
df = df %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=df$description , pattern= e),
                              new_surface))
df = df %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=df$description , pattern= f),
                              new_surface))

df = df %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=df$description , pattern= h),
                              new_surface))
df = df %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=df$description , pattern= g),
                              new_surface))

df = df %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=df$description , pattern= i),
                              new_surface))
df = df %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=df$description , pattern= j),
                              new_surface))

#recuperamos datos de 40.089 observaciones
table(is.na(df$new_surface))

#Dejamos solo como número

df = df %>% 
  mutate(surface = str_extract_all(string=df$new_surface, pattern= "[:space:]+[:digit:]+[:space:]"),
         new_surface)

df = df %>% 
  mutate(surface = ifelse(surface==	"character(0)",
                              str_extract(string=df$new_surface , pattern= "[:space:]+[:digit:]+[:punct:]+[:digit:]"),
                              surface))

#Aquellos que tengan el decimal como coma lo remplazo por punto
df = df %>% 
  mutate(surface2 = str_replace(string=df$surface , pattern= ",", replacement = "."))

#Quitamos los espacios
df = df %>% 
  mutate(surface2 = trimws(df$surface2))

#Imputamos con la variable que creamos a surfeccovered y total (todas en una)

df$area<-df$surface_total
df$area<-ifelse(is.na(df$area)==T, df$surface_covered, df$area)
df$area<-ifelse(is.na(df$area)==T, df$surface2, df$area)

table(is.na(df$area))
#Seguimos teniendo 50.912 missings
table(is.na(df$surface_total))
table(is.na(df$surface_covered))
table(is.na(df$surface))
table(is.na(df$area))

#Vamos a hacer lo mismo para ver si el apto es iluminado

df$description<-gsub("\\s+", " ", str_trim(df$description)) #quitamos dobles espacios


x = "[:space:]+iluminado+[:space:]"
y = "[:space:]+luminoso+[:space:]"
z = "[:space:]+luz+[:space:]+natural+[:space:]"

df = df %>% 
  mutate(luz = str_extract(string=df$description, pattern= x))

df = df %>% 
  mutate(luz= ifelse(is.na(luz)==T,
                              str_extract(string=df$description , pattern= y),
                              luz))
df = df %>% 
  mutate(luz= ifelse(is.na(luz)==T,
                              str_extract(string=df$description , pattern= z),
                              luz))
df = df %>% 
  mutate(iluminado= ifelse(is.na(luz)==T,0,1))

#Ahora para banos

x = "[:space:]+[:digit:]+[:space:]+baños"
df = df %>% 
  mutate(new_bano = str_extract(string=df$description , pattern= x))

table(is.na(df$new_bano))

y = "[:space:]+[:digit:]+[:space:]+banos"
df = df %>% 
  mutate(new_bano = ifelse(is.na(new_bano)==T,
                           str_extract(string=df$description , pattern= y),
                           new_bano))

z = "[:space:]+[:digit:]+[:space:]+ba&ntilde;o"
df = df %>% 
  mutate(new_bano = ifelse(is.na(new_bano)==T,
                           str_extract(string=df$description , pattern= z),
                           new_bano))

#Dejar solo número

df = df %>% 
  mutate(bano = str_extract_all(string=df$new_bano, pattern= "[:space:]+[:digit:]+[:space:]"),
         new_bano)
df = df %>% 
  mutate(bano2 = str_replace(string=df$bano , pattern= ",", replacement = "."))

df = df %>% 
  mutate(bano2 = trimws(df$bano2)) #quitar espacios
table(is.na(df$new_bano))
#Imputar

df$banos_totales<-df$bathrooms
df$banos_totales<-ifelse(is.na(df$banos_totales)==T, df$bano2, df$banos_totales)

table(is.na(df$banos_totales))
#Como seguimos teniendo muchos missings en area vamos a imputar la media de la manzana

mzn_med<-filter(manzanas_ant, MPIO_CCDGO=="05001")
mzn_bog<-manzanas_bog
leaflet() %>% addTiles() %>% addPolygons(data=mzn_med , color="red") %>% addCircles(data=df)

#Pego en una sola base de manzanas
mzn<-rbind(mzn_bog,mzn_med)
class(mzn)
#junto la base de precio de casas con esta

df_mnz = st_join(x = df ,y = mzn)

leaflet() %>%
  addTiles() %>%
  addCircles(data = df) %>%
  addPolygons(data=mzn)

colnames(df_mnz)
## average block
df_mnz = df_mnz %>%
  group_by(MANZ_CCNCT) %>%
  mutate(new_surface_2=median(surface_total,na.rm=T))

df_mnz$area2<-ifelse(is.na(df_mnz$area)==T,
                    df_mnz$new_surface_2,
                    df_mnz$area)

table(is.na(df_mnz$area2)) # solo nos quedan 10.000 missings

#Con las bases de los censos vamos a crear la variable estrat
censo<-rbind(censo_bog, censo_med)
df_mnz<-rename(df_mnz, cod_manz=MANZ_CCNCT)
censo<-rename(censo, cod_manz=COD_DANE_ANM)

table(censo$med_VA1_ESTRATO)

censo<- filter(censo,med_VA1_ESTRATO != 0.5) 
censo<- filter(censo,med_VA1_ESTRATO != 1.5) 
censo<- filter(censo,med_VA1_ESTRATO != 2.5) 
censo<- filter(censo,med_VA1_ESTRATO != 3.5) 
censo<- filter(censo,med_VA1_ESTRATO != 4.5) 
censo<- filter(censo,med_VA1_ESTRATO != 5.5) 
censo<- filter(censo,med_VA1_ESTRATO != 0) 

base<- left_join(df_mnz, censo, "cod_manz")
base<- rename(base, estrato=med_VA1_ESTRATO)
table(is.na(base$estrato))
nrow(censo)
#Machete de datos-------------------------------------------------------
#Borramos todas las variables que no necesitamos
base_final<-select(base, subset=-c(start_date, end_date, created_on,l1,l2,
                                   surface_total, surface_covered,currency,
                                   bathrooms, title, description, operation_type, 
                                   new_surface, surface, surface2, luz, new_bano, 
                                   bano, bano2, MPIO_CCDGO, CLAS_CCDGO, SETR_CCDGO, 
                                   SECR_CCDGO, CPOB_CCDGO, SETU_CCDGO, SECU_CCDGO,
                                   MANZ_CAG, ad_type, rooms, area, area2, DPTO_CCDGO, 
                                   med_V_TOT_HOG, sum_HA_TOT_PER, med_H_NRO_CUARTOS))
save(base_final, file="base_final.rda")
#KNN PARA IMPUTAR MISSINGS------------------------------------------------

#BAÑOS
base_final$banos_totales<-as.factor(base_final$banos_totales)#como factor

#Estamos especificando la base train y test, test es la que va a predecir
test<-is.na(base_final$banos_totales)
no_test<-is.na(base_final$banos_totales)==F

train<- base_final[no_test ,c("property_id","geometry", "banos_totales")]
test2<-base_final[test,c("property_id", "geometry", "banos_totales")]

#Sacamos las cordenadas para hacerlo en base a eso
train<-extract(train, geometry, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T)
test2<-extract(test2, geometry, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T)


train_labels<-train$banos_totales
dim(train[,3:4])
dim(test2[,3:4])
length(train_labels)

#Hacemos knn 

k2<-knn(train[,3:4], test2[,3:4], train_labels)

knn_bano<-cbind(test2, k1)

#Remplazo en la variable de interes y borro 
knn_bano$banos_totales<-knn_bano$k1
knn_bano<-knn_bano[,1:5]
train<-train[,1:5]
knn_bano<-rbind(train, knn_bano)
knn_bano<-select(knn_bano, subset=-c("geometry", "Lat", "Lon"))
#PARA ESTRATO

base_final$estrato<-as.factor(base_final$estrato)#como factor

#Estamos especificando la base train y test, test es la que va a predecir
test<-is.na(base_final$estrato)
no_test<-is.na(base_final$estrato)==F

train_estrato<- base_final[no_test ,c("property_id","geometry", "estrato")]
test_estrato<-base_final[test,c("property_id","geometry", "estrato")]

#Sacamos las cordenadas para hacerlo en base a eso
train_estrato<-extract(train_estrato, geometry, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T)
test_estrato<-extract(test_estrato, geometry, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T)

train_labels2<-train_estrato$estrato
dim(train_estrato[,3:4])
dim(test_estrato[,3:4])
length(train_labels2)

#Hacemos knn 

k_estrato<-knn(train_estrato[,3:4], test_estrato[,3:4], train_labels2)
knn_estrato<-cbind(test_estrato, k_estrato)

#Remplazo en la variable de interes y borro 
knn_estrato$estrato<-knn_estrato$k_estrato
knn_estrato<-knn_estrato[,1:5]
train_estrato<-train_estrato[,1:5]
knn_estrato<-rbind(train_estrato, knn_estrato)

knn_estrato<-select(knn_estrato, subset=-c("geometry", "Lat", "Lon"))
#PARA SURFACE
base_final$new_surface_2<-as.factor(base_final$new_surface_2)#como factor

#Estamos especificando la base train y test, test es la que va a predecir
test<-is.na(base_final$new_surface_2)
no_test<-is.na(base_final$new_surface_2)==F

train_surface<- base_final[no_test ,c("property_id","geometry", "new_surface_2")]
test_surface<-base_final[test,c("property_id","geometry", "new_surface_2")]

#Sacamos las cordenadas para hacerlo en base a eso
train_surface<-extract(train_surface, geometry, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T)
test_surface<-extract(test_surface, geometry, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T)

train_labels3<-train_surface$new_surface_2
dim(train_surface[,3:4])
dim(test_surface[,3:4])
length(train_labels3)

#Hacemos knn 

k_surface<-knn(train_surface[,3:4], test_surface[,3:4], train_labels3)

knn_surface<-cbind(test_surface, k_surface)
#Remplazo en la variable de interes y borro 
knn_surface$new_surface_2<-knn_surface$k_surface
knn_surface<-knn_surface[,1:5]
train_surface<-train_surface[,1:5]
knn_surface<-rbind(train_surface, knn_surface)

knn_surface<-select(knn_surface, subset=-c("geometry", "Lat", "Lon"))
#VOY A PEGAR LAS BASES KNN

impu_knn<-left_join(knn_bano, knn_estrato, "property_id")
impu_knn<-left_join(impu_knn, knn_surface, "property_id")
impu_knn<-rename(impu_knn, surface_new=new_surface_2,
                 estrato_new=estrato,
                 banos_new=banos_totales)

#La pego a base final
base_completa<-left_join(base_final, impu_knn, "property_id")
str(base_completa)
base_completa$surface_new<-as.character(base_completa$surface_new)
base_completa$banos_new<-as.character(base_completa$banos_new)
base_completa$surface_new<-as.numeric(base_completa$surface_new)
base_completa$banos_new<-as.numeric(base_completa$banos_new)

base_completa$iluminado<-factor(base_completa$iluminado)
base_completa$property_type<-factor(base_completa$property_type)
base_completa$l3<-factor(base_completa$l3)
save(base_completa, file="base_completa.rda")


#Nos quedamos solo con la base train

train_final<-filter(base_completa, base=="train")
train_final<-select(train_final, subset=-c(base))
test_final<-filter(base_completa, base=="test")
test_final<-select(test_final, subset=-c(base))

#Al train le tenemos que pegar la variabl y

precios<-select(train, property_id, price)
train_final<-left_join(precios, train_final, "property_id")

#ANALISIS DE DATOS ATIPICOS---------------------------------------------------
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

#Anlizamos para baños
boxp_est<-boxplot(log_price~estrato, data=train_final_fil, main="Log(Precio) por estrato", ylab="Log(Precio)")
boxp_ciudad<-boxplot(log_price~l3, data=train_final_fil, xlab="Ciudad", main="Log(Precio) por ciudad", ylab="Log(Precio)")
boxp_luz<-boxplot(log_price~iluminado, data=train_final_fil, xlab="Ciudad", main="Log(Precio) por luz", ylab="Log(Precio)")
boxp_baños<-boxplot(log_price~banos_new, data=train_final_fil, xlab="No. baños", main="Log(Precio) por no. baños", ylab="Log(Precio)")#borramos los que tengan demasiadps

#Vamos a eliminar las observaciones con más de nueve años

train_final_fil2<-filter(train_final_fil, banos_new<9)
boxp_baños2<-boxplot(log_price~banos_new, data=train_final_fil2, xlab="No. baños", main="Log(Precio) por No. baños", ylab="Log(Precio)")
train_final_fil2<-filter(train_final_fil2, bedrooms>0)
boxp_bedr<-boxplot(log_price~bedrooms, data=train_final_fil2, xlab="No. dormitorios", main="Log(Precio) por No. dormitorios", ylab="Log(Precio)")


table(train_final_fil$bedrooms)
#Analizamos misssings
t<-filter(base_final, base=="train")
missings<-as.data.frame(map(base_final, ~sum(is.na(.))))

missings<-as.data.frame(map(train_final_fil2, ~sum(is.na(.))))

#Distancias a lugares de interes---------------------------------------------

#separamos entre ciudades

tbog<-filter(train_final_fil2, l3== "Bogotá D.C")
tmed<-filter(train_final_fil2, l3== "Medellín")

test_bog<-filter(test_final, l3== "Bogotá D.C")
test_med<-filter(test_final, l3== "Medellín")
