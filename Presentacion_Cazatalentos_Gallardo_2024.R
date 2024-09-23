# CODIGO CAZATALENTOS
#AUSTRAL EDICION 2024 PRESENCIAL
# ALUMNO GALLARDO EZEQUIEL

library(rpart)
library(caret)  # Para validación cruzada
library(lightgbm)  # Para usar el modelo LightGBM

set.seed(470213)

# INICIO CAZATALENTO 1:--------------------------------------------------------------------------------------

Cantidad_Alumnos <- 100 
Tiros_Por_Alumno <- 100 

# Función para simular los tiros de un alumno
tirar_libres <- function(prob, qty) {
  tiros <- runif(qty) 
  return(sum(tiros < prob))  # Cuenta los encestes 
}

# Generar probabilidades dinámicamente
alumnos <- sprintf("1%03d", 1:Cantidad_Alumnos)  
probabilidades <- numeric()  # Vector para almacenar las p
prob <- 0.80  # p inicial

# Generar las p hasta completar la cantidad de alumnos
while (length(probabilidades) < Cantidad_Alumnos) {
  n <- ifelse(length(probabilidades) + 2 <= Cantidad_Alumnos, 2, Cantidad_Alumnos - length(probabilidades))  
  probabilidades <- c(probabilidades, rep(prob, n)) 
  prob <- prob - 0.01
}

tabla_alumnos <- data.frame(id = alumnos, probabilidad = probabilidades)

# Simular el proceso de tiros por alumno
resultados <- matrix(0, nrow = Cantidad_Alumnos, ncol = Tiros_Por_Alumno)

# Simulación: cada alumno tira según su p
for (i in 1:Tiros_Por_Alumno) {
  resultados[, i] <- sapply(tabla_alumnos$probabilidad, function(prob) tirar_libres(prob, Tiros_Por_Alumno))
}

resultados_df <- as.data.frame(resultados)

if (any(is.na(resultados_df))) {
  resultados_df <- na.omit(resultados_df)  
}

resultados_df$probabilidad <- tabla_alumnos$probabilidad

resultados_df$total_aciertos <- rowSums(resultados_df[, 1:Tiros_Por_Alumno])  # Sumar aciertos por alumno

# Configuración de la validación cruzada para RPART
control <- trainControl(method = "cv", number = 5)  # Validación cruzada con 5 pliegues

# Entrenar el modelo rpart 
modelo_rpart <- train(probabilidad ~ total_aciertos, data = resultados_df, method = "rpart", trControl = control)

# Predecir la p utilizando el modelo entrenado
predicciones <- predict(modelo_rpart, newdata = resultados_df)

tabla_alumnos$prediccion <- predicciones

mejor_alumno <- tabla_alumnos[which.max(tabla_alumnos$prediccion),]
mejor_alumno$cazatalento <- "Cazatalento1"

# Crear la tabla final "Selección"
seleccion <- data.frame(alumno = mejor_alumno$id, 
                        probabilidad = mejor_alumno$probabilidad, 
                        prediccion = mejor_alumno$prediccion,
                        cazatalento = mejor_alumno$cazatalento)

# FIN CAZATALENTO 1:--------------------------------------------------------------------------------------

# INICIO CAZATALENTO 2:--------------------------------------------------------------------------------------
Cantidad_Alumnos <- 200 
Tiros_Por_Alumno <- 100 

# Mantener las mismas probabilidades que en el cazatalento 1
alumnos_cazador2 <- sprintf("2%03d", 1:Cantidad_Alumnos)

# Crear la tabla de alumnos para cazador 2
tabla_alumnos2 <- data.frame(id = alumnos_cazador2, probabilidad = probabilidades)

# Simulación: cada alumno tira según sup
resultados2 <- matrix(0, nrow = Cantidad_Alumnos, ncol = Tiros_Por_Alumno)

# Simular tiros
for (i in 1:Tiros_Por_Alumno) {
  resultados2[, i] <- sapply(tabla_alumnos2$probabilidad, function(prob) tirar_libres(prob, Tiros_Por_Alumno))
}

resultados_df2 <- as.data.frame(resultados2)
if (any(is.na(resultados_df2))) {
  resultados_df2 <- na.omit(resultados_df2)
}

resultados_df2$probabilidad <- tabla_alumnos2$probabilidad
resultados_df2$total_aciertos <- rowSums(resultados_df2[, 1:Tiros_Por_Alumno])

# Configuración de la validación cruzada para LightGBM
dtrain <- lgb.Dataset(data = as.matrix(resultados_df2$total_aciertos), label = resultados_df2$probabilidad)

params <- list(objective = "regression", metric = "rmse", num_leaves = 31)

# Entrenar el modelo LightGBM
modelo_lgb <- lgb.train(params = params, data = dtrain, nrounds = 100)
predicciones_lgb <- predict(modelo_lgb, as.matrix(resultados_df2$total_aciertos))

tabla_alumnos2$prediccion <- predicciones_lgb

mejor_alumno2 <- tabla_alumnos2[which.max(tabla_alumnos2$prediccion),]
mejor_alumno2$cazatalento <- "Cazatalento2"

seleccion2 <- data.frame(alumno = mejor_alumno2$id, 
                         probabilidad = mejor_alumno2$probabilidad, 
                         prediccion = mejor_alumno2$prediccion,
                         cazatalento = mejor_alumno2$cazatalento)

# FIN CAZATALENTO 2:--------------------------------------------------------------------------------------

# INICIO CAZATALENTO 3:--------------------------------------------------------------------------------------

# Definir las variables iniciales
Cantidad_Alumnos <- 100
Tiros_Por_Alumno <- 100


tirar_libres <- function(prob, qty) {
  tiros <- runif(qty)  
  return(sum(tiros < prob))  
}

# Generar probabilidades dinámicamente
alumnos <- sprintf("3%03d", 1:Cantidad_Alumnos)  
probabilidades <- numeric() 
prob <- 0.80  

while (length(probabilidades) < Cantidad_Alumnos) {
  n <- ifelse(length(probabilidades) + 2 <= Cantidad_Alumnos, 2, Cantidad_Alumnos - length(probabilidades))  
  probabilidades <- c(probabilidades, rep(prob, n))  # Añadir probabilidad a pares de alumnos
  prob <- prob - 0.01
}

tabla_alumnos <- data.frame(id = alumnos, probabilidad = probabilidades)

alumno_seleccionado <- sample(1:Cantidad_Alumnos, 1)

alumno_elegido <- tabla_alumnos[alumno_seleccionado, ]
aciertos_alumno_elegido <- tirar_libres(alumno_elegido$probabilidad, Tiros_Por_Alumno)
alumno_elegido$prediccion <- alumno_elegido$probabilidad
alumno_elegido$cazatalento <- "Cazatalento3"

seleccion_cazatalento3 <- data.frame(alumno = alumno_elegido$id, 
                                     probabilidad = alumno_elegido$probabilidad, 
                                     prediccion = alumno_elegido$prediccion,
                                     cazatalento = alumno_elegido$cazatalento)

# FIN CAZATALENTO 3:--------------------------------------------------------------------------------------

# INICIO CAZATALENTO 4: -------------------------------------------------------------------------------
library(ranger)

# Definir las variables iniciales
Cantidad_Alumnos <- 2 
Tiros_Por_Alumno <- 100

# Probabilidades medias para los alumnos
media_probabilidades <- c(0.80, 0.75)  
desviacion_estandar <- 0.05


alumnos <- sprintf("4%03d", 1:Cantidad_Alumnos)

tabla_alumnos <- data.frame(id = alumnos, probabilidad = media_probabilidades)

resultados <- matrix(0, nrow = Cantidad_Alumnos, ncol = Tiros_Por_Alumno)
for (i in 1:Tiros_Por_Alumno) {
  resultados[, i] <- sapply(1:Cantidad_Alumnos, function(j) {
    prob_aleatoria <- rnorm(1, mean = tabla_alumnos$probabilidad[j], sd = desviacion_estandar)
    prob_aleatoria <- min(max(prob_aleatoria, 0), 1)  
    return(tirar_libres(prob_aleatoria, 1))  
  })
}

resultados_df <- as.data.frame(resultados)
resultados_df$probabilidad <- tabla_alumnos$probabilidad
resultados_df$total_aciertos <- rowSums(resultados_df[, 1:Tiros_Por_Alumno])  # Sumar aciertos por alumno

modelo_ranger <- ranger(probabilidad ~ total_aciertos, data = resultados_df, num.trees = 100)

predicciones <- predict(modelo_ranger, data = resultados_df)$predictions
tabla_alumnos$prediccion <- predicciones

mejor_alumno <- tabla_alumnos[which.max(tabla_alumnos$prediccion),]

mejor_alumno$cazatalento <- "Cazatalento4"

seleccion_cazatalento4 <- data.frame(alumno = mejor_alumno$id, 
                                     probabilidad = mejor_alumno$probabilidad, 
                                     prediccion = mejor_alumno$prediccion,
                                     cazatalento = mejor_alumno$cazatalento)


# FIN CAZATALENTO 4: ---------------------------------------------------------------------------------


# INICIO CAZATALENTO 5: --------------------------------------------------------------------------------

# Cargar las librerías necesarias
library(caret)  
library(rpart)  

Cantidad_Alumnos <- 500  
Tiros_Por_Alumno <- 10   
Lotes <- 10              
alumnos <- sprintf("5%03d", 1:Cantidad_Alumnos)  # IDs para el cazatalento 5

# Crear un data frame para almacenar la información de los alumnos
probabilidades <- c(0.80, runif(Cantidad_Alumnos - 1, 0.5, 0.79)) 
tabla_alumnos <- data.frame(id = alumnos, probabilidad = probabilidades)

resultados <- matrix(0, nrow = Cantidad_Alumnos, ncol = Lotes * Tiros_Por_Alumno)
for (lote in 1:Lotes) {
  for (tiro in 1:Tiros_Por_Alumno) {
    resultados[, (lote - 1) * Tiros_Por_Alumno + tiro] <- sapply(tabla_alumnos$probabilidad, function(prob) {
      rbinom(1, 1, prob)  # Simulación de tiro libre (1 éxito/0 fallo)
    })
  }
}

resultados_df <- as.data.frame(resultados)
resultados_df$probabilidad <- tabla_alumnos$probabilidad

resultados_df$total_aciertos <- rowSums(resultados_df[, 1:(Lotes * Tiros_Por_Alumno)])  # Sumar aciertos por alumno

control <- trainControl(method = "cv", number = 5)

grid <- expand.grid(cp = seq(0.01, 0.1, by = 0.01))

modelo_grid <- train(probabilidad ~ total_aciertos, data = resultados_df, method = "rpart", 
                     trControl = control, tuneGrid = grid)

predicciones <- predict(modelo_grid, newdata = resultados_df)

tabla_alumnos$prediccion <- predicciones

mejor_alumno <- tabla_alumnos[which.max(tabla_alumnos$prediccion),]
mejor_alumno$cazatalento <- "Cazatalento5"
seleccion_cazatalento5 <- data.frame(alumno = mejor_alumno$id, 
                                     probabilidad = mejor_alumno$probabilidad, 
                                     prediccion = mejor_alumno$prediccion,
                                     cazatalento = mejor_alumno$cazatalento)

# FIN CAZATALENTO 5: ----------------------------------------------------------------------------------

# INICIO ELECCION CAZATALENTO: ----------------------------------------------------------------------

eleccion_cazatalento <- rbind(seleccion_cazatalento5, 
                              seleccion_cazatalento4, 
                              seleccion_cazatalento3, 
                              seleccion2, 
                              seleccion)

eleccion_cazatalento <- eleccion_cazatalento[order(-eleccion_cazatalento$prediccion),]

# Mostrar el dataset final de elecciones
print(eleccion_cazatalento)

