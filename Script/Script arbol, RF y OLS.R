###Problem Set 3

load("~/Maestría/MATERIAS/Big data and machine learning/Problem sets/Taller 3/dataPS3/scompleta.rda")
install.packages("tree")
install.packages("ISLR")

library(MASS)
library(tree)
library(ISLR)

install.packages("randomForest")
library(randomForest)

set.seed(1111)

#Crear base de entrenamiento y prueba

sample <- sample(c(TRUE, FALSE), nrow(scompleta), replace=TRUE, prob=c(0.7,0.3))
training  <- scompleta[sample, ]
testing   <- scompleta[!sample, ]

### Modelo 1 - Árbol
set.seed(1111)

arbol1 <- tree(log_price ~ l3+bedrooms+property_type+iluminado+banos_new+
               estrato_new+surface_new+dist_estacion+distancia_mall, training)
arbol1
summary(arbol1)

# Cross validation
cvarbol1 <-cv.tree(arbol1)
plot(cvarbol1$size, cvarbol1$dev, type="b")

#Test del CV
yhat1<- predict(arbol1, newdata = testing)
Predichoarbol1<- testing$log_price
plot(yhat1, Predichoarbol1)
abline (0,1)
mean((yhat1- Predichoarbol1)^2) #Test MSE

###Modelo 2- Random forest
set.seed(1111)
rforest<- randomForest(log_price~ l3+bedrooms+property_type+iluminado+banos_new+
                         estrato_new+surface_new+dist_estacion+distancia_mall, training,
                         ntree=20, importance=TRUE)

#Test del Random Forest
yhat3<- predict(rforest, newdata=testing)
plot(yhat3, Predichoarbol1)
abline (0,1)
mean((yhat3- Predichoarbol1)^2) #Test MSE
importance(rforest)
varImpPlot(rforest)

###Modelo 3 - OLS con todas las variables
Reg1<- lm(log_price~ l3+bedrooms+property_type+iluminado+banos_new+
            estrato_new+surface_new+dist_estacion+distancia_mall, data = training)
summary(Reg1)

yhat4<- predict(Reg1, newdata = testing)
mean((yhat4- Predichoarbol1)^2) #Test MSE


###Modelo 4 - OLS con dos variables no lineales
Reg2<- lm(log_price~ I(banos_new^2)+I(bedrooms^2), data = training) 
summary(Reg2)

yhat5<- predict(Reg2, newdata = testing)
mean((yhat5- Predichoarbol1)^2) #Test MSE


###Modelo 5 - OLS con interacciones
Reg3<- lm(log_price~ l3+l3*bedrooms+l3*property_type+l3*iluminado+l3*banos_new+
            l3*estrato_new+l3*surface_new+l3*dist_estacion+l3*distancia_mall, data = training)
summary(Reg3)
yhat6<- predict(Reg3, newdata = testing)
mean((yhat6- Predichoarbol1)^2) #Test MSE

stargazer(Reg1, Reg2, Reg3, type="html", title= "Precio de vivienda", align=TRUE, out="models.latex")

###Predicción CSV
predicciones<-data.frame(test$property_id)
predicciones$randomforest<-predict(rforest, newdata=test_completa)
table(is.na(predicciones$randomforest))   

save(predicciones2,file = "Predicciones_forest.csv")
write.csv(predicciones2, file="Predicciones_RF.csv")



