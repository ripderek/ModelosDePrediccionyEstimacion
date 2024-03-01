#EV PARCIAL SKERE MODO DIABLO

library(nnet)
library(caret)
library(randomForest) 
library(tidyverse)

install.packages("randomForest")


fname = "C:/INTELIGENCIA DE NEGOCIOS/PREPROCESAMIENTO/SemillasNew.txt"


dfSemillas_original = read.table(fname, sep=",", dec=".", na.strings = "?", header = TRUE)



train_control <- trainControl(method = "cv",
                              number = 10)
variablesNNET <- expand.grid(size = 10,decay=10e-2)

# predecir la variable objetivo
# según el clasificador nnent
modelo.nb <- train(Species~., data = dfSemillas_original,
                   method = "nnet",
                   trControl = train_control,
                   metric="RMSE",
                   tuneGrid = variablesNNET)

#para random forest 
model_forest <- train(Species ~ ., data = dfSemillas_original, method = "rf", trControl = train_control,metric="RMSE")

# Obtener resultados
results <- resamples(list(nnet = model_nnet, randomForest = model_forest))
summary(results)


print(modelo.nb)
Species.predecido <- predict(modelo.nb, dfSemillas_original[,-8])



#Obtiene los valores predecidos de la variable dependiente "Species"
Species.predecido <- predict (modelo.nb, dfSemillas_original); 

Species.predecido_forest <- predict (model_forest, dfSemillas_original); 


confusionMatrix(as.factor(dfSemillas_original$Species),as.factor(Species.predecido_forest))





#Segundo Ejercicio 
#Considérese la función no lineal dada por y = 2x3-3x2-3x+2. Se conoce que el dominio de x
#está definido entre [-2,3]. Se pide hallar un modelo para estimación de la función usando
#SVM.
library(e1071)

# Definir la función
funcion <- function(x) {2*x^3 - 3*x^2 - 3*x + 2}

# Generar datos de entrenamiento
set.seed(123)
x_train <- runif(100, min = -2, max = 3)
y_train <- funcion(x_train)

# Entrenar el modelo SVM
modelo <- svm(y_train ~ x_train, kernel = "radial", cost = 1)

# Generar datos de prueba
x_test <- seq(-2, 3, length.out = 100)
y_test <- funcion(x_test)

# Predecir con el modelo SVM
y_pred <- predict(modelo, data.frame(x_train = x_test))

# Graficar los resultados
plot(x_test, y_test, type = "l", col = "blue", ylim = c(-20, 20), xlab = "x", ylab = "y")
points(x_train, y_train, col = "red")
points(x_test, y_pred, col = "black", lty = 20)
legend("topleft", legend = c("Real", "Predicho", "Entrenamiento"), col = c("blue", "green", "red"), lty = c(1, 2, 1))



plot(x_test, y_test, type = "l", col = "blue", ylim = c(-20, 20), xlab = "x", ylab = "y")
points(x_train, y_train, col = "red")
points(x_test, y_pred, col = "black", lty = 20)
legend("topleft", legend = c("Real", "Predicho", "Entrenamiento"), col = c("blue", "green", "red"), lty = c(1, 2, 1))




