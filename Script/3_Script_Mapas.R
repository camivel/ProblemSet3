p_load(tidyverse, # Manipular dataframes
       rio, # Importar datos
       stringr, # Manipular texto
       stargazer, # Output de las regresiones
       rgeos, # Calcular centroides
       plotly, # Gráficos dinámicos
       sf, # Leer/escribir/manipular datos espaciales
       leaflet, # Visualizaciones dinámicas
       tmaptools, # geocode_OSM()
       osmdata) # Get OSM's data

###TRANSPORTE - ESTACIONES------------------------------------------------------

###Medellín
available_tags("public_transport")

#Localizamos estaciones de metro
estacionesm <- opq(bbox = getbb("Medellin Colombia")) %>%
  add_osm_feature(key = "public_transport" , value = "station") 
estacionesm_sf <- osmdata_sf(estacionesm) #Lo convierte a formato sf para dibujar
estacionesm_geometria <- estacionesm_sf$osm_polygons %>% 
  select(osm_id, name) #Se le crea el poligono
#Calculamos centroides y mapa
centroides_estm <- gCentroid(as(estacionesm_geometria$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = estacionesm_geometria, col = "green",
              opacity = 0.8, popup = estacionesm_geometria$name) %>%
  addCircles(lng = centroides_estm$x, 
             lat = centroides_estm$y, 
             col = "red", opacity = 1, radius = 1)

#Distancias de las viviendas a la estación más cercana
#convertimos el data frame enn tipo sf
tmed2<-extract(tmed, geometry, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T)
tmed_sf <- st_as_sf(tmed2, coords = c("Lat", "Lon"))
st_crs(tmed_sf) <- 4326 #ambas bases deben tener el mimo geodetic c
centroides_estm_sf <- st_as_sf(centroides_estm, coords = c("x", "y"))
st_crs(centroides_estm_sf) <- 4326

dist_matrix_med <- st_distance(x = tmed_sf , y = centroides_estm_sf)

# Encontramos la distancia mínima a una estación
dist_min_estm <- apply(dist_matrix_med, 1, min)
tmed_sf$distancia_estmetro <- dist_min_estm

#Bogotá (lo mismo)
estacionesb <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "public_transport" , value = "station") 
estacionesb_sf <- osmdata_sf(estacionesb)
estacionesb_geometria <- estacionesb_sf$osm_polygons %>% 
  select(osm_id, name)

centroides_estb <- gCentroid(as(estacionesb_geometria$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = estacionesb_geometria, col = "green",
              opacity = 0.8, popup = estacionesb_geometria$name) %>%
  addCircles(lng = centroides_estb$x, 
             lat = centroides_estb$y, 
             col = "red", opacity = 1, radius = 1)

#Distancia a la estación más cercana
#convertimos el data frame enn tipo sf
tbog2<-extract(tbog, geometry, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T)
tbog_sf <- st_as_sf(tbog2, coords = c("Lat", "Lon"))
st_crs(tbog_sf) <- 4326 
centroides_estb_sf <- st_as_sf(centroides_estb, coords = c("x", "y"))
st_crs(centroides_estb_sf) <- 4326

dist_matrix_bog <- st_distance(x = tbog_sf , y = centroides_estb_sf)

# Encontramos la distancia mínima a una estación
dist_min_estb <- apply(dist_matrix_bog, 1, min)
tbog_sf$distancia_esttransm <- dist_min_estb
### SHOPS----------------------------------------------------------------------

#Medellín
available_tags("shop")

mallm <- opq(bbox = getbb("Medellin Colombia")) %>%
  add_osm_feature(key = "shop" , value = "mall") 
mallm_sf <- osmdata_sf(mallm)
mallm_geometria <- mallm_sf$osm_polygons %>% 
  select(osm_id, name)

centroides_mallm <- gCentroid(as(mallm_geometria$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = mallm_geometria, col = "purple",
              opacity = 0.8, popup = mallm_geometria$name) %>%
  addCircles(lng = centroides_mallm$x, 
             lat = centroides_mallm$y, 
             col = "blue", opacity = 1, radius = 1)
#Calculamos la distancia mínima al mall 
centroides_mallm_sf <- st_as_sf(centroides_mallm, coords = c("x", "y"))
st_crs(centroides_mallm_sf) <- 4326

dist_matrix_med2 <- st_distance(x = tmed_sf , y = centroides_mallm_sf)

# Encontramos la distancia mínima a un cc
dist_min_mallm <- apply(dist_matrix_med2, 1, min)
tmed_sf$distancia_mall <- dist_min_mallm

#Bogotá
mallb <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "shop" , value = "mall") 
mallb_sf <- osmdata_sf(mallb)
mallb_geometria <- mallb_sf$osm_polygons %>% 
  select(osm_id, name)

centroides_mallb <- gCentroid(as(mallb_geometria$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = mallb_geometria, col = "purple",
              opacity = 0.8, popup = mallb_geometria$name) %>%
  addCircles(lng = centroides_mallb$x, 
             lat = centroides_mallb$y, 
             col = "blue", opacity = 1, radius = 1)

#Calculamos la distancia mínima al mall 
centroides_mallb_sf <- st_as_sf(centroides_mallb, coords = c("x", "y"))
st_crs(centroides_mallb_sf) <- 4326

dist_matrix_bog2 <- st_distance(x = tbog_sf , y = centroides_mallb_sf)

# Encontramos la distancia mínima a un cc
dist_min_mallb <- apply(dist_matrix_bog2, 1, min)
tbog_sf$distancia_mall <- dist_min_mallb




