
#ESTRATO CENSO 2018-------------------------------------------------------------

#BOGOTA

setwd ("C:/Users/Diana Contreras/OneDrive - Universidad de los Andes/1.Big Data/Problem Set 3/11_Bogota/11Bogota/11_Bogota_CSV")
## load data
mgn = import("CNPV2018_MGN_A2_11.CSV")
colnames(mgn)
distinct_all(mgn[,c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA")]) %>% nrow()

hog = import("CNPV2018_2HOG_A2_11.CSV")
colnames(hog)
distinct_all(hog[,c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA","H_NROHOG")]) %>% nrow()

viv = import("CNPV2018_1VIV_A2_11.CSV") 
colnames(viv)
distinct_all(viv[,c("COD_ENCUESTAS","U_VIVIENDA")]) %>% nrow()

## join data
viv_hog = left_join(hog,viv,by=c("COD_ENCUESTAS","U_VIVIENDA","UA_CLASE"))
table(is.na(viv_hog$VA1_ESTRATO))

data = left_join(viv_hog,mgn,by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))
table(is.na(data$VA1_ESTRATO))

## select vars
H_NRO_CUARTOS = "Número de cuartos en total"
HA_TOT_PER = "Total personas en el hogar"
V_TOT_HOG = "Total de hogares en la vivienda"
VA1_ESTRATO = "Estrato de la vivienda (según servicio de energía)"
COD_DANE_ANM = "Codigo DANE de manzana"

db = data %>% select(COD_DANE_ANM,H_NRO_CUARTOS,HA_TOT_PER,V_TOT_HOG,VA1_ESTRATO)

## summary data
censo = db %>%
  group_by(COD_DANE_ANM) %>% 
  summarise(med_H_NRO_CUARTOS=median(H_NRO_CUARTOS,na.rm=T), 
            sum_HA_TOT_PER=sum(HA_TOT_PER,na.rm=T), 
            med_V_TOT_HOG=median(V_TOT_HOG,na.rm=T),
            med_VA1_ESTRATO=median(VA1_ESTRATO,na.rm=T))

## export data
export(censo,"censo_bog.rds")

##MEDELLIN----------------------------------------------------------------------

setwd ("C:/Users/Diana Contreras/OneDrive - Universidad de los Andes/1.Big Data/Problem Set 3/05_Antioquia/05Antioquia/05_Antioquia_CSV")
## load data
mgn2 = import("CNPV2018_MGN_A2_05.CSV")
mgn2 <- filter(mgn2, U_MPIO==1)
distinct_all(mgn2[,c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA")]) %>% nrow()

hog2 = import("CNPV2018_2HOG_A2_05.CSV")
hog2 <- filter(hog2, U_MPIO==1)
colnames(hog2)
distinct_all(hog2[,c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA","H_NROHOG")]) %>% nrow()

viv2 = import("CNPV2018_1VIV_A2_05.CSV") 
viv2 <- filter(viv2, U_MPIO==1)
colnames(viv2)
distinct_all(viv2[,c("COD_ENCUESTAS","U_VIVIENDA")]) %>% nrow()

## join data
viv_hog2 = left_join(hog2,viv2,by=c("COD_ENCUESTAS","U_VIVIENDA","UA_CLASE"))
table(is.na(viv_hog2$VA1_ESTRATO))

data2 = left_join(viv_hog2,mgn2,by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))
table(is.na(data$VA1_ESTRATO))

## select vars
H_NRO_CUARTOS = "Número de cuartos en total"
HA_TOT_PER = "Total personas en el hogar"
V_TOT_HOG = "Total de hogares en la vivienda"
VA1_ESTRATO = "Estrato de la vivienda (según servicio de energía)"
COD_DANE_ANM = "Codigo DANE de manzana"

db2 = data2 %>% select(COD_DANE_ANM,H_NRO_CUARTOS,HA_TOT_PER,V_TOT_HOG,VA1_ESTRATO)

## summary data
censo_m = db2 %>%
  group_by(COD_DANE_ANM) %>% 
  summarise(med_H_NRO_CUARTOS=median(H_NRO_CUARTOS,na.rm=T), 
            sum_HA_TOT_PER=sum(HA_TOT_PER,na.rm=T), 
            med_V_TOT_HOG=median(V_TOT_HOG,na.rm=T),
            med_VA1_ESTRATO=median(VA1_ESTRATO,na.rm=T))

## export data

readRDS()
export(censo_m,"censo_med.rds")