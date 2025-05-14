

# PCA - R (3 dimensiones)
# Autor: Diego Vértiz Padilla

# Este script aplica PCA a un conjunto de datos con 3 columnas numéricas.
# Se centra la matriz, se calcula la matriz de covarianza y correlación,
# y se proyectan los datos en 2D para visualizar la varianza explicada.

library(readr)
library(dplyr)
library(ggplot2)
library(scatterplot3d)

# Cargar datos
datos <- read_csv("indicadores_datos.csv")
datos_num <- select_if(datos, is.numeric)


#Graficamos los datos originales
scatterplot3d(
  x = as.numeric(datos_num[[1]]),
  y = as.numeric(datos_num[[2]]),
  z = as.numeric(datos_num[[3]]),
  color = "darkgreen", pch = 19,
  xlab = colnames(datos_num)[1],
  ylab = colnames(datos_num)[2],
  zlab = colnames(datos_num)[3],
  main = "Datos Originales (3 Indicadores)"
)

# Centrar 
datos_centered <- scale(datos_num, center = TRUE, scale = FALSE)

# Covarianza
print("Matriz de covarianza:")
print(cov(datos_centered))

# Correlación
print("Matriz de correlación:")
print(cor(datos_centered))

pca_result <- prcomp(datos_centered, center = FALSE, scale. = FALSE)

var_exp <- summary(pca_result)$importance[2,]
var_acum <- cumsum(var_exp)

print("Varianza explicada:")
print(var_exp)
print("Varianza acumulada:")
print(var_acum)

# Gráfico varianza acumulada
plot(var_acum, type = "b", pch = 19, col = "blue",
     xlab = "Componentes principales", ylab = "Varianza acumulada",
     main = "PCA - Varianza acumulada")

# Proyección 2D
scores <- as.data.frame(pca_result$x)
ggplot(scores, aes(x = PC1, y = PC2)) +
  geom_point(color = "steelblue") +
  labs(title = "Proyección PCA en 2D", x = "PC1", y = "PC2") +
  theme_minimal()
